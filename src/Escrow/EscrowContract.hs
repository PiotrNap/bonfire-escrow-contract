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

module Escrow.EscrowContract (validator, validatorHash) where

import Escrow.Types
import Ledger (contains)
import qualified Ledger.Ada as Ada
import Plutus.Script.Utils.V1.Typed.Scripts.Validators (DatumType, RedeemerType)
import Plutus.Script.Utils.V2.Typed.Scripts (TypedValidator, ValidatorTypes, mkTypedValidatorParam, mkUntypedValidator, validatorHash, validatorScript)
import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
 
{-# INLINEABLE mkValidator #-}
mkValidator :: EscrowParam -> EscrowDatum -> EventAction -> ScriptContext -> Bool
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
               lovelaceByFeeRate = round (fromInteger totalLovelaceSpent * (1 `unsafeRatio` 100))
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

---

data EscrowTypes

instance ValidatorTypes EscrowTypes where
  type DatumType EscrowTypes = EscrowDatum
  type RedeemerType EscrowTypes = EventAction

typedValidator :: EscrowParam -> TypedValidator EscrowTypes
typedValidator = go
  where
    go =
      mkTypedValidatorParam @EscrowTypes
        $$(PlutusTx.compile [||mkValidator||])
        $$(PlutusTx.compile [||wrap||])
    wrap = mkUntypedValidator

validator :: EscrowParam -> Validator
validator = validatorScript . typedValidator

escrowValidatorHash :: EscrowParam -> ValidatorHash
escrowValidatorHash = validatorHash . typedValidator
