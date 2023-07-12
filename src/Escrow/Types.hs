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

data EventEscrowDatum = EventEscrowDatum
  {
    beneficiaryPkh :: !PubKeyHash,
    benefactorPkh :: !PubKeyHash,
    releaseDate :: !POSIXTime,
    cancelDeadline :: !POSIXTime,
    createdAt :: !POSIXTime
  }

PlutusTx.unstableMakeIsData ''EventEscrowDatum

data EventAction = Cancel | Complete | Recycle
  deriving (Show)

PlutusTx.makeIsDataIndexed ''EventAction [('Cancel, 0), ('Complete, 1),('Recycle, 2) ]
PlutusTx.makeLift ''EventAction
