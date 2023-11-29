module GetJSON (
  decodeJson
) where

-- Imports

import Data.Aeson ( FromJSON, decode )
import qualified Data.ByteString.Lazy as B

-- Functions

decodeJson :: (FromJSON jsonType) => FilePath -> IO (Maybe jsonType)
decodeJson jsonFilePath =
  decode <$> rawJson
  where
    rawJson = B.readFile jsonFilePath
