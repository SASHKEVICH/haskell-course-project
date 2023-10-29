{-# LANGUAGE DeriveGeneric #-}

module Contraindications (
  Contraindication(..)
  , getContraindicationsByIds
) where

-- Imports

import Prelude hiding ( id )
import GHC.Generics
import Data.Aeson
import Data.List ( find )

import GetJSON ( decodeJson )
import FromJust ( fromJust )

-- Declararations

data Contraindication = Contraindication
  { id :: Int
    , russian_name :: String
  } deriving (Show, Generic)


newtype ContraindicationsJSON = ContraindicationsJSON
  { contraindications :: [Contraindication]
  } deriving (Show, Generic)


instance FromJSON Contraindication
instance FromJSON ContraindicationsJSON

-- Functions

jsonFile :: FilePath
jsonFile = "./db/contraindications.json"


getAllContraindications :: IO [Contraindication]
getAllContraindications = do
  decodedContraindicationsJSON <- decodeJson jsonFile :: IO(Maybe ContraindicationsJSON)

  case decodedContraindicationsJSON of
    Just contraindicationsJSON -> return $ contraindications contraindicationsJSON
    Nothing -> return []


getContraindicationsByIds :: [Int] -> IO [Contraindication]
getContraindicationsByIds ids = do
  allContraindications <- getAllContraindications
  return $ findContraindicationsByIds allContraindications ids


findContraindicationsByIds :: [Contraindication] -> [Int] -> [Contraindication]
findContraindicationsByIds allContraindications ids =
  [ fromJust $ find (\contraindication -> id contraindication == contraindicationId) allContraindications | contraindicationId <- ids ]

