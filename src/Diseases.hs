{-# LANGUAGE DeriveGeneric #-}

module Diseases (
    Disease
    , DiseasesJSON
    , getJSON
) where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B

data Reciept = Reciept
  { plant_id :: Int
  , amount :: Float
  } deriving (Show, Generic)

data Disease = Disease 
  { id :: Int
  , russian_name :: String
  , latin_name :: String
  , reciept :: [Reciept]
  , duration :: Int    
  } deriving (Show, Generic)

newtype DiseasesJSON = DiseasesJSON 
  { diseases :: [Disease]
  } deriving (Show, Generic)

instance FromJSON Reciept
instance FromJSON Disease
instance FromJSON DiseasesJSON

jsonFile :: FilePath
jsonFile = "./db/diseases.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile
