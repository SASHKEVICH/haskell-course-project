{-# LANGUAGE DeriveGeneric #-}
module Plants (
  Plant,
  price,
  getPlantsByIds
) where

-- Imports

import Prelude hiding (id)
import Data.List (find)
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B

-- Declarations

data Plant = Plant
  { id :: Int
    , latin_name :: String
    , russian_name :: String
    , price :: Int
    ,  information :: String
    , growing_area :: [Int]
    , contraindications :: [Int]
    , diseases :: [Int]
  } deriving (Show, Generic)

newtype PlantsJSON = PlantsJSON
  { plants :: [Plant]
  } deriving (Show, Generic)

instance FromJSON Plant
instance FromJSON PlantsJSON

-- Functions

jsonFile :: FilePath
jsonFile = "./db/plants.json"


getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile


getAllPlants :: IO [Plant]
getAllPlants = do
  decodedPlantsJSON <- (decode <$> getJSON) :: IO (Maybe PlantsJSON)

  case decodedPlantsJSON of
    Just plantsJson -> return $ plants plantsJson
    Nothing -> return []


getPlantsByIds :: [Int] -> IO [Maybe Plant]
getPlantsByIds ids = do
  allPlants <- getAllPlants

  let filteredPlants = findPlantsByIds allPlants ids
  return filteredPlants


findPlantsByIds :: [Plant] -> [Int] -> [Maybe Plant]
findPlantsByIds allPlants ids =
  [ find (\plant -> id plant == plantId) allPlants | plantId <- ids ]