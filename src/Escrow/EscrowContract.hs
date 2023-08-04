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

module Escrow.EscrowContract (validator, validatorHash ) where

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

-- This validator should meet the following requirements:
--
-- 1. Allow to cancel a transaction by one of two parties transacting with each other
-- 2. Allow to cancel only before the cancellation deadline
-- 3. Ensure all the funds are returned to benefactor during on-time cancellation 
-- 4. Prevent from collecting funds before release date
-- 5. Allow to collect funds after release date by beneficiary
-- 6. Check whether sufficient fee was sent to the treasury (if any)
-- 7. Allow to recycle UTxOs older than 1 year by a treasury
-- 8. All recycled UTxOs must be returned to the treasury

{-# INLINEABLE mkValidator #-}
mkValidator :: EscrowParam -> EscrowDatum -> EventAction -> ScriptContext -> Bool
mkValidator param datum action ctx =
  case action of
    Cancel ->
      traceIfFalse "Cancellation Tx must be sign by one of two parties" (signedBy benefactor || signedBy beneficiary)
       && traceIfFalse "The cancellation deadline has passed" beforeCancelDeadline
        && traceIfFalse "Output must be sent to benefactor" (sufficientOutputTo benefactor)
    Complete ->
        traceIfFalse "Completion Tx must be sign by beneficiary" (signedBy beneficiary)
        && traceIfFalse "It is too early to collect" afterReleaseDate
        && traceIfFalse "Output must be sent to beneficiary" (sufficientOutputTo beneficiary)
        && traceIfFalse "Fee must be sent to the treasury" sufficientFeeToTreasury
    Recycle ->
        traceIfFalse "Recycle Tx must be signed by the treasury" (signedBy treasury)
        && traceIfFalse "UTxO must be older than 1 year" utxoOlderThanOneYear
        && traceIfFalse "Recycle Tx must be sent to the treasury" (sufficientOutputTo treasury)
  where

    --- Variables ---

    info = scriptContextTxInfo ctx
    totalValueSpent = valueSpent info
    inVals = symbols totalValueSpent
    benefactor = benefactorPkh datum
    beneficiary = beneficiaryPkh datum
    treasury = treasuryPkh param
    ownInput = findOwnInput ctx

    --- Functions ---

    serviceFeeValue :: Value
    serviceFeeValue =
        if isBetaTesterTokenPresent || isNothing ownInput
        then Ada.lovelaceValueOf 0
        else
            let
            totalLovelaceSpent = Ada.getLovelace $ Ada.fromValue $ paymentTokens datum
            lovelaceByFeeRate = round (fromInteger totalLovelaceSpent * (1 `unsafeRatio` 100))
            in
             if lovelaceByFeeRate <  minServiceFeeLovelace param
             then Ada.lovelaceValueOf (minServiceFeeLovelace param)
             else Ada.lovelaceValueOf lovelaceByFeeRate

    sufficientOutputTo :: PubKeyHash -> Bool
    sufficientOutputTo pkh = valuePaidTo info pkh `geq` paymentTokens datum

    signedBy :: PubKeyHash -> Bool
    signedBy = txSignedBy info

    isBetaTesterTokenPresent :: Bool
    isBetaTesterTokenPresent = betaTesterToken param `elem` inVals

    beforeCancelDeadline :: Bool
    beforeCancelDeadline = contains (to $ cancelDeadline datum) $ txInfoValidRange info

    afterReleaseDate :: Bool
    afterReleaseDate = contains (from $ releaseDate datum) $ txInfoValidRange info

    sufficientFeeToTreasury :: Bool
    sufficientFeeToTreasury = isBetaTesterTokenPresent || (valuePaidTo info treasury == serviceFeeValue)

    utxoOlderThanOneYear :: Bool
    utxoOlderThanOneYear =
      let
         dateOfCreation = createdAt datum
         oneYearLater = dateOfCreation + POSIXTime (60 * 60 * 24 * 365)
      in
         contains (from oneYearLater) (txInfoValidRange info)

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
