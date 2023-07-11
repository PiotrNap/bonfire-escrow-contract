{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}

module Escrow.Types where

import Ledger hiding (singleton)
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
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
    paymentAssets :: [(AssetClass, Integer)]
  }

PlutusTx.unstableMakeIsData ''EventEscrowDatum

data EventAction = Cancel | Complete | Recycle
  deriving (Show)

PlutusTx.makeIsDataIndexed ''EventAction [('Cancel, 0), ('Complete, 1),('Recycle, 2) ]
PlutusTx.makeLift ''EventAction
