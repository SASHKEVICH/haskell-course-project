{-# LANGUAGE DeriveGeneric #-}

module GrowingAreas (

) where

-- Imports

import GHC.Generics
import Data.Aeson

-- Declararations

data GrowingArea = GrowingArea
  { id :: Int
    , russian_name :: String
    , location :: String
  } deriving (Show, Generic)


newtype GrowingAreasJSON = GrowingAreasJSON
  { growing_areas :: [GrowingArea]
  } deriving (Show, Generic)


instance FromJSON GrowingArea
instance FromJSON GrowingAreasJSON

-- Functions


