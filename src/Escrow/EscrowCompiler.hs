{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Escrow.EscrowCompiler (writeParameratizedValidator, writeJson) where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Escrow.EscrowContract
import Escrow.Types
import qualified Ledger
import Plutus.V1.Ledger.Api (Data (B, Constr, I, List, Map), ToData, toData, unValidatorScript)
import GHC.IO
import Data.Either
import PlutusTx.Prelude ((<$>), ($), Functor (fmap), (.), Maybe (Nothing))

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (I n) = ScriptDataNumber n
dataToScriptData (B b) = ScriptDataBytes b
dataToScriptData (Map xs) = ScriptDataMap [(dataToScriptData k, dataToScriptData v) | (k, v) <- xs]
dataToScriptData (List xs) = ScriptDataList $ fmap dataToScriptData xs

writeJson :: ToData a => FilePath -> a -> IO ()
writeJson file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . toData

writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . unValidatorScript

writeParameratizedValidator :: IO (Either (FileError ()) ())
writeParameratizedValidator =
  writeValidator "output/plutus-scripts/escrow.plutus" $ validator exampleParam
