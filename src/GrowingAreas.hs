{-# LANGUAGE DeriveGeneric #-}

module GrowingAreas (
  GrowingArea(..)
  , getAllAreas
  , getAreasByIds
  , sortByFirstGrowingAreaCondition
  , findAreasByIds
) where

-- Imports

import Prelude hiding ( id )
import GHC.Generics
import Data.Aeson
import Data.List ( find )

import FromJust ( fromJust )
import GetJSON ( decodeJson )

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

jsonFile :: FilePath
jsonFile = "./db/growing_areas.json"


getAllAreas :: IO [GrowingArea]
getAllAreas = do
  decodedAreasJSON <- decodeJson jsonFile :: IO (Maybe GrowingAreasJSON)

  case decodedAreasJSON of
    Just areasJSON -> return $ growing_areas areasJSON
    Nothing -> return []


getAreasByIds :: [Int] -> IO [GrowingArea]
getAreasByIds ids = do
  allAreas <- getAllAreas
  return $ findAreasByIds allAreas ids


findAreasByIds :: [GrowingArea] -> [Int] -> [GrowingArea]
findAreasByIds allAreas ids = [ fromJust (find (\area -> id area == areaId) allAreas) | areaId <- ids ]


sortByFirstGrowingAreaCondition :: GrowingArea -> GrowingArea -> Ordering
sortByFirstGrowingAreaCondition lt rt
  | russian_name lt > russian_name rt = GT
  | otherwise = LT
