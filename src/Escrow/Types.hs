{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Escrow.Types where

import Ledger hiding (singleton)
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude (Show (..))

data EscrowParam = EscrowParam
  {
    treasuryPkh :: !PubKeyHash
    betaTesterToken: !CurrencySymbol
  }

PlutusTx.makeLift ''EscrowParam

data EventEscrowDatum = EventEscrowDatum
  {
    beneficiaryPkh :: !PubKeyHash,
    benefactorPkh :: !PubKeyHash,
    releaseDate :: !POSIXTime
    cancelDeadline :: !POSIXTime
    paymentAssets :: [Value]
  }

-- think about leveled auth/access NFTs for Organizers that unlock new payment tiers... (2022-04-06)
-- Organizer's scheduling window is an account preference stored in Bonfire DB

PlutusTx.unstableMakeIsData ''EventEscrowDatum

data EventAction = Cancel | Complete -- ?? | Dispute
  deriving (Show)

PlutusTx.makeIsDataIndexed ''EventAction [('Cancel, 0), ('Complete, 1)] -- ('Dispute, 3)]
PlutusTx.makeLift ''EventAction

---- Dispute Contract
----
--data DisputeParam = DisputeParam
--  { adminToken :: !CurrencySymbol,
--    dpPtSymbol :: !CurrencySymbol,
--    dpPtName :: !TokenName,
--    treasuryPkh :: !PubKeyHash
--  }

-- PlutusTx.makeLift ''DisputeParam

-- data DisputeDatum = DisputeDatum
--   { bddOrganizerPkh :: !PubKeyHash,
--     bddAttendeePkh :: !PubKeyHash,
--     bddEventCostLovelace :: !Integer,
--     bddEventCostPaymentToken :: !Integer,
--     bddEventID :: !BuiltinByteString,
--     bddDisputeID :: !BuiltinByteString
--   }

-- PlutusTx.unstableMakeIsData ''DisputeDatum

-- data DisputeResult = PayAttendee | PayOrganizer | Split

-- PlutusTx.makeIsDataIndexed ''DisputeResult [('PayAttendee, 0), ('PayOrganizer, 1), ('Split, 2)]
-- PlutusTx.makeLift ''DisputeResult
