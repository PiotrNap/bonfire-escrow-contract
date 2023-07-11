{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Escrow.EscrowContract where

import Escrow.Types
import Ledger hiding (singleton)
import Ledger.Ada
import Ledger.Typed.Scripts
import Ledger.Value as Value
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import PlutusTx.Prelude as Plutus
  ( Bool (..),
    Eq ((==)),
    Integer,
    Maybe (..),
    fromInteger,
    length,
    traceIfFalse,
    unsafeRatio,
    (&&),
    (*),
    (.),
    (<>),
    (>=),
  )
import PlutusTx.Ratio (numerator)

--  check if the withdraw is being signed to the beneficiary:  v/
--  allow cancellation only before the deadline: v/
--  don't charge a fee during cancellation:
--  withdraw possible after release date:  v/
--  withdraw tx made by the benefactor: v/
--  check if benefactor has Beta-Tester token: v/
--  if user holds a beta tester token, don't charge any fee:
--  ...otherwise charge a fee: 
--  add a way for service provider to withdraw utxo's older than 1 year: 
--  add a 1.5% fee or a 1.5 ADA fee if it's less than that

{-# INLINEABLE mkValidator #-}
mkValidator :: EscrowParam -> EventEscrowDatum -> EventAction -> ScriptContext -> Bool
mkValidator param datum action ctx =
  case action of
    Cancel ->
      traceIfFalse "Cancellation Tx must be sign by one of two parties" signedByOneParty
        && traceIfFalse "The Cancellation deadline has passed" beforeCancelDeadline
        && traceIfFalse "Output must be fully returned" sufficientOutputToInitiator
    Complete ->
      traceIfFalse "Beneficiary must sign the withdraw Tx" signedByBeneficiary
        && traceIfFalse "It is too early to collect" afterReleaseDate
        && traceIfFalse "Output must be fully withdrawn" sufficientOutputToInitiator
  where
    --- Variables ---
    
    info :: TxInfo
    info = scriptContextTxInfo ctx

    cancelDeadline :: POSIXTime
    cancelDeadline = cancelDeadline datum

    totalValueSpent :: Value
    totalValueSpent = valueSpent info

    inVals :: [CurrencySymbol]
    inVals = symbols totalValueSpent

    valueToBenefactor :: Value
    valueToBenefactor = valuePaidTo info $ benefactorPkh datum

    valueToBeneficiary :: Value
    valueToBeneficiary = valuePaidTo info $ beneficiaryPkh datum

    valueToTreasury :: Value
    valueToTreasury = valuePaidTo info $ treasuryPkh param

    serviceFee :: Value -> Integer
    serviceFee = 
    let
        totalLovelace = Ada.fromValue totalValueSpent
    in
        if totalLovelace > Ada.lovelaceOf 5
        then Ada.toValue (totalLovelace `div` 100 * 15)
        else Ada.toValue (Ada.lovelaceOf 1.5)

    --- Functions ---

    signedByBeneficiary :: Bool
    signedByOrganizer = txSignedBy info $ beneficiaryPkh datum

    signedByBenefactor :: Bool
    signedByOrganizer = txSignedBy info $ benefactorPkh datum

    signedByOneParty :: Bool
    signedByOneParty =  signedByBeneficiary || signedByBenefactor

    beforeCancelDeadline :: Bool
    beforeCancelDeadline = contains (to cancelDeadline) $ txInfoValidRange datum

    afterReleaseDate :: Bool
    afterReleaseDate = inside (releaseDate datum) (txInfoValidRange info)

    isBetaTesterTokenPresent :: Bool
    isBetaTesterTokenPresent = (betaTesterToken param) `elem` inVals


    -- Handle Outputs --

    -- new
    let fee = if isBetaTesterTokenPresent then 0 else calculateFee (valueSpent info)
    allLovelaceReturned = valuePaidTo info (pubKeyHash $ txOutPubKey $ head $ getContinuingOutputs ctx) (Value.singleton Ada.adaSymbol Ada.adaToken 1) >= (totalLovelace - fee)
    allAssetsReturned = all (\v -> valuePaidTo info (pubKeyHash $ txOutPubKey $ head $ getContinuingOutputs ctx) v >= valueOf v) paymentAssets
    allReturned = allLovelaceReturned && allAssetsReturned
    
    
    -- old
    allAssetsReturned = all (\v -> valuePaidTo info (pubKeyHash $ txOutPubKey $ head $ getContinuingOutputs ctx) v >= valueOf v) paymentAssets datum
    correctFeePaid = let
        totalLovelace = sum $ fmap (\i -> assetClassValueOf (txOutValue $ txInInfoResolved i) (AssetClass (Ada.adaSymbol, Ada.adaToken))) (txInfoInputs info)
        requiredFee = if totalLovelace > 5000000 then (totalLovelace `div` 100) * 15 else 1500000
        in valuePaidTo info treasuryPkh (Value.singleton Ada.adaSymbol Ada.adaToken requiredFee) >= requiredFee
    
    --  very old
    sufficientOutputToInitiator :: Bool
    sufficientOutputToInitiator =
      (getLovelace $ fromValue valueToBenefactor) >= (eventCostLovelace datum)
        && (valueOf valueToBenefactor (ptSymbol param) (ptName param)) >= (eventCostPaymentToken datum)

    -- Returns True if at least 2 ADA fee is paid, or if fee is at least 5% of event cost.
    -- This approach allows for options on front-end while maintining guarantee that Organizer gets at least (cost - 2) or (cost * 95%)
    -- The "threshold" for minimum event cost can therefore be "set" to anything.
    -- Think about front-end messaging and how to handle event cost of 35 ada, for example.
    --
    -- ...ideally we don't want to rely on any third party to decide how much to charge

    lovelaceOutputsCorrect :: Bool
    lovelaceOutputsCorrect =
      ( fromInteger (getLovelace $ fromValue valueToBeneficiary) >= (95 `unsafeRatio` 100) * fromInteger (eventCostLovelace datum)
          && fromInteger (getLovelace $ fromValue valueToTreasury) >= (5 `unsafeRatio` 100) * fromInteger (eventCostLovelace datum)
      )
        || ( getLovelace (fromValue valueToBeneficiary) >= (eventCostLovelace datum - 2000000)
               && getLovelace (fromValue valueToTreasury) >= 2000000
           )

    -- vvv This isn't good, what if the utxo has a bit less in it... say user sent something, event didn't got booked, and now organizer 
    -- vvv can't even withdraw...
    -- gimbal costs can always stick to 95% / 5%
    gimbalOutputsCorrect :: Bool
    gimbalOutputsCorrect =
      fromInteger (valueOf valueToBeneficiary (ptSymbol param) (ptName param)) >= (95 `unsafeRatio` 100) * fromInteger (eventCostPaymentToken datum)
        && fromInteger (valueOf valueToTreasury (ptSymbol param) (ptName param)) >= (5 `unsafeRatio` 100) * fromInteger (eventCostPaymentToken datum)

    sufficientOutputToBeneficiary :: Bool
    sufficientOutputToBeneficiary = gimbalOutputsCorrect && lovelaceOutputsCorrect

data EscrowTypes

instance ValidatorTypes EscrowTypes where
  type DatumType EscrowTypes = EventEscrowDatum
  type RedeemerType EscrowTypes = EventAction

typedValidator :: EscrowParam -> TypedValidator EscrowTypes
typedValidator param =
  mkTypedValidator @EscrowTypes
    ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode param)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = wrapValidator @EventEscrowDatum @EventAction

validator :: EscrowParam -> Validator
validator = validatorScript . typedValidator
