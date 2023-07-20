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

-- This validator should meet the following requirements:
--
-- 1. Allow to cancel a transaction by one of two parties transacting with each other
-- 2. Allow to cancel only before the cancellation deadline
-- 3. Ensure all the funds are returned to benefactor during on-time cancellation 
-- 4. Prevent from collecting funds before release date by anyone
-- 5. Allow to collect funds after release date by beneficiary
-- 6. Check whether sufficient fee was sent to treasury (if any)
-- 7. Allow to recycle UTxOs older than 1 year by a treasury
-- 8. All recycled UTxOs must be returned to the treasury

{-# INLINEABLE mkValidator #-}
mkValidator :: EscrowParam -> EscrowDatum -> EventAction -> ScriptContext -> Bool
mkValidator param datum action ctx =
  case action of
    Cancel ->
      traceIfFalse "Cancellation Tx must be sign by one of two parties" (signedByBeneficiary || signedByBenefactor)
       && traceIfFalse "The cancellation deadline has passed" beforeCancelDeadline
        && traceIfFalse "Output must be sent to benefactor" sufficientOutputToBenefactor
    Complete ->
        traceIfFalse "It is too early to collect" afterReleaseDate
        && traceIfFalse "Output must be sent to beneficiary" (valueToBeneficiary - serviceFeeValue == valueProduced info - serviceFeeValue)
        && traceIfFalse "Fee must be sent to the treasury" sufficientFeeToTreasury
    Recycle ->
        traceIfFalse "Recycle Tx must be signed by the treasury" signedByTreasury
        && traceIfFalse "UTxO must be older than 1 year" utxoOlderThanOneYear
        && traceIfFalse "Recycle Tx must be sent to the treasury" (valueToTreasury == valueProduced info)
  where
    --- Variables ---

    info :: TxInfo
    info = scriptContextTxInfo ctx

    totalValueSpent :: Value
    totalValueSpent = valueSpent info

    totalOwnValueSpent :: Value

    inVals :: [CurrencySymbol]
    inVals = symbols totalValueSpent

    valueToBeneficiary :: Value
    valueToBeneficiary = valuePaidTo info $ beneficiaryPkh datum

    valueToTreasury :: Value
    valueToTreasury = valuePaidTo info $ treasuryPkh param

    ownInput :: Maybe TxInInfo
    ownInput = findOwnInput ctx

    --
    -- ! This is wrong. 'totalValueSpent' is not (always) equal to the amount of funds
    -- being unlocked from the script.
    -- 
    -- Solution : calculate fee based on the total ownInput's Value,
    -- if 'serviceFeeValue' < totalOutputToTreasury then False else True
    serviceFeeValue :: Value
    serviceFeeValue =
      if isBetaTesterTokenPresent || isNothing ownInput
           then Ada.lovelaceValueOf 0
           else
             let
               totalLovelaceSpent = Ada.getLovelace $ Ada.fromValue totalOwnValueSpent
               lovelaceByFeeRate = round (fromInteger totalLovelaceSpent * (1 `unsafeRatio` 100))
             in
                if lovelaceByFeeRate <  minServiceLovelaceFee
                then Ada.lovelaceValueOf minServiceLovelaceFee
                else Ada.lovelaceValueOf lovelaceByFeeRate

    --- Functions ---

    filterInputsWithDatums :: ScriptContext -> [TxInInfo]
    filterInputsWithDatums =
        let inputs = txInfoInputs $ scriptContextTxInfo ctx
        in filter (isJust . txOutDatum . txInInfoResolved) inputs

    getTxInInfoValue :: TxInInfo -> Value
    getTxInInfoValue txInInfo = txOutValue $ txInInfoResolved txInInfo

    currentOutputTo :: PubKeyHash -> Bool
    currentOutputTo pkh = case ownInput of
                Just input -> getTxInInfoValue input `geq` valuePaidTo info  pkh -- this assumes that value paid to someone is less than spending script's output -> WRONG
                Nothing -> False

    sufficientOutputToBenefactor :: Bool
    sufficientOutputToBenefactor = currentOutputTo $ benefactorPkh datum

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

    sufficientFeeToTreasury :: Bool
    sufficientFeeToTreasury = valueToTreasury == serviceFeeValue

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
