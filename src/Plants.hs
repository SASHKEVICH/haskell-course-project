{-# LANGUAGE DeriveGeneric #-}
module Plants (
  Plant,
  price,
  getPlantsByIds,
  showPlantsTreatingDisease
) where

-- Imports

import Prelude hiding ( id )
import Data.List ( find )
import Data.Aeson
import GHC.Generics
import Text.Show.Unicode ( uprint )
import System.Exit ( exitSuccess )
import qualified Data.ByteString.Lazy as B

import TryAgain ( tryAgain )

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


showPlantsTreatingDisease :: Int -> IO ()
showPlantsTreatingDisease diseaseId = do
  allPlants <- getAllPlants

  let plantsTreatingDisease = findAllPlantsTreatingDisease allPlants diseaseId
  uprint plantsTreatingDisease

  mainMenu


findAllPlantsTreatingDisease :: [Plant] -> Int -> [Plant]
findAllPlantsTreatingDisease allPlants diseaseId =
  [ plant | plant <- allPlants, doesPlantTreatDisease plant ]
  where
    doesPlantTreatDisease plant = elem diseaseId $ diseases plant


mainMenu :: IO ()
mainMenu = do
  putStrLn "\nМеню:"
  putStrLn "1. Показать полную информацию о растении"
  putStrLn "2. Отсортировать растения по цене"
  putStrLn "3. Отсортировать растения по ареалу произрастания"
  putStrLn "4. Выйти в начало"
  putStrLn "5. Выйти из программы"

  decision <- getLine

  case decision of
    "1" -> exitSuccess
    "2" -> exitSuccess
    "3" -> exitSuccess
    _ -> tryAgain mainMenu
