{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}

module Escrow.Types where

import Ledger hiding (singleton)
import qualified PlutusTx
import Prelude (Show (..))

data EscrowParam = EscrowParam
  {
    treasuryPkh :: !PubKeyHash,
    betaTesterToken :: !CurrencySymbol
  }

PlutusTx.makeLift ''EscrowParam

-- Testnet params
exampleParam :: EscrowParam
exampleParam = EscrowParam
         { 
            treasuryPkh = "3475d4d36f40048a934ddc211dd84fa7c00a910421b566fc7e886080",
            betaTesterToken = "481146d15d0c9bacc880254f88f944f6a88dba2e917d35fcbf92aa24"
         }

data EscrowDatum = EscrowDatum
  {
    beneficiaryPkh :: !PubKeyHash,
    benefactorPkh :: !PubKeyHash,
    releaseDate :: !POSIXTime,
    cancelDeadline :: !POSIXTime,
    createdAt :: !POSIXTime
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
        createdAt = 1689247512
    }

