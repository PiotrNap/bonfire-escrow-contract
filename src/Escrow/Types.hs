{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}

module Escrow.Types where

import Ledger hiding (singleton)
import qualified PlutusTx
import Prelude (Show (..))
import PlutusTx.Prelude
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import Plutus.V1.Ledger.Api

data EscrowParam = EscrowParam
  {
    treasuryPkh :: PubKeyHash,
    betaTesterToken :: CurrencySymbol,
    minServiceFeeLovelace :: Integer
  }

PlutusTx.makeLift ''EscrowParam

-- Testnet params
exampleParam :: EscrowParam
exampleParam = EscrowParam
  { 
     treasuryPkh = "3475d4d36f40048a934ddc211dd84fa7c00a910421b566fc7e886080",
     betaTesterToken = "481146d15d0c9bacc880254f88f944f6a88dba2e917d35fcbf92aa24",
     minServiceFeeLovelace = 1_500_000
  }

data EscrowDatum = EscrowDatum
  {
    beneficiaryPkh :: PubKeyHash,
    benefactorPkh :: PubKeyHash,
    releaseDate :: POSIXTime,
    cancelDeadline :: POSIXTime,
    createdAt :: POSIXTime,
    paymentTokens :: Value
  }

PlutusTx.unstableMakeIsData ''EscrowDatum

data EventAction = Cancel | Complete | Recycle
  deriving (Show)

PlutusTx.makeIsDataIndexed ''EventAction [('Cancel, 0), ('Complete, 1),('Recycle, 2) ]
PlutusTx.makeLift ''EventAction

exampleDatum :: EscrowDatum
exampleDatum = EscrowDatum 
    {
        beneficiaryPkh = "a763d63377c24431eeb1544b4a6933f9e813a8122b5e15b904608569",
        benefactorPkh = "5729dc6d950bdd7cf60a5c4169119ff2a70f3a8f56b93b056a7eabaf",
        releaseDate = 1689251008,
        cancelDeadline = 1689249628,
        createdAt = 1689247512,
        paymentTokens = totalValue
    }


-- Create a CurrencySymbol for the PIGGY token
piggySymbol :: CurrencySymbol
piggySymbol = CurrencySymbol $ stringToBuiltinByteString "3e544e20875172fe302df3afdcdaefeba828299e0f89562449845a4f"

-- Create a TokenName for the PIGGY token
piggyToken :: TokenName
piggyToken = TokenName $ stringToBuiltinByteString "PIGGY"

-- Create a Value with 100 ADA
adaValue :: Value
adaValue = singleton adaSymbol adaToken 25_000_000

-- Create a Value with 1000 PIGGY tokens
piggyValue :: Value
piggyValue = singleton piggySymbol piggyToken 1000

-- Combine the ADA and PIGGY Values
totalValue :: Value
totalValue = adaValue + piggyValue
