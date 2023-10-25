module GetJSON (
  decodeJson
) where

-- Imports

import Data.Aeson
import qualified Data.ByteString.Lazy as B

-- Functions

getJSON :: FilePath -> IO B.ByteString
getJSON = B.readFile


decodeJson :: (FromJSON jsonType) => FilePath -> IO (Maybe jsonType)
decodeJson jsonFilePath =
  decode <$> rawJson
  where rawJson = getJSON jsonFilePath
