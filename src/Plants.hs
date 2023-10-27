{-# LANGUAGE DeriveGeneric #-}

module Plants (
  Plant(..)
  , getPlantsByIds
  , getAllPlants
  , findAllPlantsTreatingDisease
  , findPlantWithId
) where

-- Imports

import Prelude hiding ( id )
import Data.List ( find )
import Data.Aeson
import GHC.Generics

import GetJSON ( decodeJson )

-- Declarations

data Plant = Plant
  { id :: Int
    , latin_name :: String
    , russian_name :: String
    , price :: Int
    , information :: String
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


getAllPlants :: IO [Plant]
getAllPlants = do
  decodedPlantsJSON <- decodeJson jsonFile :: IO(Maybe PlantsJSON)

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


findAllPlantsTreatingDisease :: [Plant] -> Int -> [Plant]
findAllPlantsTreatingDisease allPlants diseaseId =
  [ plant | plant <- allPlants, doesPlantTreatDisease plant ]
  where
    doesPlantTreatDisease plant = elem diseaseId $ diseases plant


findPlantWithId :: Int -> IO (Maybe Plant)
findPlantWithId plantId = do
  plantsById <- getPlantsByIds [plantId]
  return $ head plantsById
