{-# LANGUAGE DeriveGeneric #-}

module Contraindications (
  getContraindicationsByIds
) where

-- Imports

import Prelude hiding ( id )
import GHC.Generics
import Data.Aeson
import Data.List ( find )

import GetJSON ( decodeJson )

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


getContraindicationsByIds :: [Int] -> IO [Maybe Contraindication]
getContraindicationsByIds ids = do
  allContraindications <- getAllContraindications
  return $ findContraindicationsByIds allContraindications ids


findContraindicationsByIds :: [Contraindication] -> [Int] -> [Maybe Contraindication]
findContraindicationsByIds allContraindications ids =
  [ find (\contraindication -> id contraindication == contraindicationId) allContraindications | contraindicationId <- ids ]

