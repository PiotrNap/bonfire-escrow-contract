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
import qualified GHC.Real as PlutusTx.Prelude

--  add a way for service provider to withdraw utxo's older than 1 year: 

{-# INLINEABLE mkValidator #-}
mkValidator :: EscrowParam -> EventEscrowDatum -> EventAction -> ScriptContext -> Bool
mkValidator param datum action ctx =
  case action of
    Cancel ->
        traceIfFalse "The cancellation deadline has passed" beforeCancelDeadline
        && traceIfFalse "Output must be fully returned to benefactor" sufficientOutputToBenefactor
    Complete ->
        traceIfFalse "It is too early to collect" afterReleaseDate
        && traceIfFalse "Output must be fully withdrawn" sufficientOutputToBeneficiary
    Recycle ->
        traceIfFalse "Recycle Tx must be signed by the treasury" signedByTreasury
        && traceIfFalse "UTxO must be older than 1 year" utxoOlderThanOneYear
  where
    --- Variables ---
    
    info :: TxInfo
    info = scriptContextTxInfo ctx

    totalValueSpent :: Value
    totalValueSpent = valueSpent info

    inVals :: [CurrencySymbol]
    inVals = symbols totalValueSpent

    totalLovelace :: Integer
    totalLovelace = sum $ fmap (\i -> assetClassValueOf (txOutValue $ txInInfoResolved i) (AssetClass (Ada.adaSymbol, Ada.adaToken))) (txInfoInputs info)

    nonLovelacePaymentAssets :: [Value]
    nonLovelacePaymentAssets = filter (\v -> assetClassValueOf v (AssetClass (Ada.adaSymbol, Ada.adaToken)) == 0) (paymentAssets datum)

    valueToBenefactor :: Value
    valueToBenefactor = valuePaidTo info $ benefactorPkh datum

    valueToBeneficiary :: Value
    valueToBeneficiary = valuePaidTo info $ beneficiaryPkh datum

    -- valueToTreasury :: Value
    -- valueToTreasury = valuePaidTo info $ treasuryPkh param

    serviceFee :: Value 
    serviceFee = 
      if isBetaTesterTokenPresent 
           then Ada.toValue 0
           else 
               let
                   _totalLovelace = Ada.fromValue totalValueSpent
               in
                   if _totalLovelace > Ada.lovelaceOf 5
                   then Ada.lovelaceValueOf (Ada.getLovelace _totalLovelace `PlutusTx.Prelude.div` 100 * 15)
                   else Ada.lovelaceValueOf (Ada.getLovelace 1.5)
    --- Functions ---

    isBetaTesterTokenPresent :: Bool
    isBetaTesterTokenPresent = betaTesterToken param `elem` inVals

    beforeCancelDeadline :: Bool
    beforeCancelDeadline = contains (to $ cancelDeadline datum) $ txInfoValidRange info

    afterReleaseDate :: Bool
    afterReleaseDate = contains (from $ releaseDate datum) $ txInfoValidRange info

    allLovelaceReturned :: Value -> Bool
    allLovelaceReturned totalValue = 
      let
        adaAssetClass = AssetClass (Ada.adaSymbol, Ada.adaToken)
        (_currencySymbol, _tokenName) = unAssetClass adaAssetClass
        adaInTotalValue = valueOf totalValue _currencySymbol _tokenName
        serviceFeeInLovelace = Ada.getLovelace $ Ada.fromValue serviceFee
      in
        adaInTotalValue >= totalLovelace - serviceFeeInLovelace

    allAssetsReturned :: Value -> Bool
    allAssetsReturned totalValue = all (\(assetClass, amount) -> valueOf totalValue (unAssetClass assetClass) >= amount) (paymentAssets datum)

    sufficientOutputToBeneficiary :: Bool
    sufficientOutputToBeneficiary = allAssetsReturned valueToBeneficiary && allLovelaceReturned valueToBeneficiary 

    sufficientOutputToBenefactor :: Bool
    sufficientOutputToBenefactor = valueSpent == valueToBenefactor

    signedByTreasury :: Bool
    signedByTreasury = txSignedBy info $ treasuryPkh param

    utxoOlderThanOneYear :: Bool
    utxoOlderThanOneYear = 
        let
          txInfoTimeRange = txInfoValidRange info
          oneYearAgo = POSIXTime $ getPOSIXTime $ from txInfoTimeRange - 60 * 60 * 24 * 365
        in
          contains (from oneYearAgo) txInfoTimeRange


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
