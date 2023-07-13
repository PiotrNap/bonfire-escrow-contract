{-# LANGUAGE OverloadedStrings #-}

module Escrow.DataToJson where

import Cardano.Api
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import PlutusTx (Data (..))
import qualified PlutusTx
import Prelude (IO)
import System.IO (FilePath)
import Escrow.Types
import PlutusTx.Prelude

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs) = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs) = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n) = ScriptDataNumber n
dataToScriptData (B bs) = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeUnit :: IO ()
writeUnit = writeJSON "output/bonfire-testnet/unit.json" ()

writeExampleDatum :: IO ()
writeExampleDatum = writeJSON "output/datums/example-datum.json" exampleDatum
