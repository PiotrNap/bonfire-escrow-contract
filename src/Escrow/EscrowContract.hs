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
{-# LANGUAGE NumericUnderscores #-}

module Escrow.EscrowContract where

import Escrow.Types
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import Ledger.Typed.Scripts
import Ledger.Value as Value
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import PlutusTx.Prelude()
import PlutusTx.Ratio()
import qualified Plutus.V2.Ledger.Api as PlutusV2
 
{-# INLINEABLE mkValidator #-}
mkValidator :: EscrowParam -> EscrowDatum -> EventAction -> PlutusV2.ScriptContext -> Bool
mkValidator param datum action ctx =
  case action of
    Cancel ->
      traceIfFalse "Cancellation Tx must be sign by one of two parties" (signedByBeneficiary || signedByBenefactor)
       && traceIfFalse "The cancellation deadline has passed" beforeCancelDeadline
        && traceIfFalse "Output must be fully returned to benefactor" sufficientOutputToBenefactor
    Complete ->
        traceIfFalse "Beneficiary must sign the withdraw Tx" signedByBeneficiary
        && traceIfFalse "It is too early to collect" afterReleaseDate
        && traceIfFalse "Output must be fully withdrawn" (sufficientOutputToBeneficiary && sufficientOutputToTreasury) -- fee applied
    Recycle ->
        traceIfFalse "Recycle Tx must be signed by the treasury" signedByTreasury
        && traceIfFalse "UTxO must be older than 1 year" utxoOlderThanOneYear
        && traceIfFalse "Recycle Tx must be sent to the treasury" sufficientOutputToTreasury
  where
    --- Variables ---

    info :: TxInfo
    info = scriptContextTxInfo ctx

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

    serviceFee :: Integer
    serviceFee = 
      if isBetaTesterTokenPresent
           then 0
           else
             let
               totalLovelaceSpent = Ada.getLovelace $ Ada.fromValue totalValueSpent
               lovelaceByFeeRate = round (fromInteger totalLovelaceSpent * (minServiceLovelaceFee `unsafeRatio` 100))
             in 
                if lovelaceByFeeRate <  minServiceLovelaceFee
                then minServiceLovelaceFee
                else lovelaceByFeeRate

    --- Functions ---
   
    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ beneficiaryPkh datum

    signedByBenefactor :: Bool
    signedByBenefactor = txSignedBy info $ benefactorPkh datum

    signedByTreasury :: Bool
    signedByTreasury = txSignedBy info $ treasuryPkh param

    isBetaTesterTokenPresent :: Bool
    isBetaTesterTokenPresent = betaTesterToken param `elem` inVals

    beforeCancelDeadline :: Bool
    beforeCancelDeadline = contains (to $ cancelDeadline datum) $ txInfoValidRange info

    afterReleaseDate :: Bool
    afterReleaseDate = contains (from $ releaseDate datum) $ txInfoValidRange info

    sufficientOutputToBeneficiary :: Bool
    sufficientOutputToBeneficiary = 
          let serviceFeeValue = Ada.lovelaceValueOf serviceFee 
          in totalValueSpent - serviceFeeValue == valueToBeneficiary - serviceFeeValue

    sufficientOutputToBenefactor :: Bool
    sufficientOutputToBenefactor = totalValueSpent == valueToBenefactor

    sufficientOutputToTreasury :: Bool
    sufficientOutputToTreasury =
      case action of 
        Recycle -> totalValueSpent == valueToTreasury
        Complete -> valueToTreasury == Ada.lovelaceValueOf serviceFee

    utxoOlderThanOneYear :: Bool
    utxoOlderThanOneYear =
      let
         dateOfCreation = createdAt datum
         oneYearLater = dateOfCreation + POSIXTime (60 * 60 * 24 * 365)
      in
         contains (from oneYearLater) (txInfoValidRange info)

--- Helper Functions / Variables ---

minServiceLovelaceFee :: Integer
minServiceLovelaceFee = 1_500_000

data EscrowTypes

instance ValidatorTypes EscrowTypes where
  type DatumType EscrowTypes = EscrowDatum
  type RedeemerType EscrowTypes = EventAction

typedValidator :: EscrowParam -> PlutusV2.TypedValidator EscrowTypes
typedValidator param =
  Utils.mkTypedValidator @EscrowTypes
    ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode param)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = wrapValidator @EscrowDatum @EventAction

validator :: EscrowParam -> PlutusV2.Validator
validator = validatorScript . typedValidator
