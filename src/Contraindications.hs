{-# LANGUAGE DeriveGeneric #-}

module Contraindications (

) where

-- Imports

import GHC.Generics
import Data.Aeson

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


