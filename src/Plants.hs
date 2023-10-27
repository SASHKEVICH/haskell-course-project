{-# LANGUAGE DeriveGeneric #-}

module Plants (
  Plant(..)
  , getPlantsByIds
  , getAllPlants
  , findAllPlantsTreatingDisease
  , showSortedPlantsFlow
  , findPlantWithId
) where

-- Imports

import Prelude hiding ( id )
import Data.List ( find, sortBy )
import Data.Aeson
import GHC.Generics
import Text.Show.Unicode ( uprint, ushow )

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


-- showInformationAboutPlantFlow :: [Plant] -> IO ()
-- showInformationAboutPlantFlow plantsTreatingDisease = do
--   putStrLn "Введите id растения:\n"

--   decision <- getLine

--   let plantId = readMaybe decision :: Maybe Int
--   case plantId of
--     Just realPlantId -> tryShowInformationAboutPlantFlow realPlantId
--     Nothing -> do
--       putStrLn "Введите число"
--       showInformationAboutPlantFlow plantsTreatingDisease


-- tryShowInformationAboutPlantFlow :: Int -> IO ()
-- tryShowInformationAboutPlantFlow plantId = do
--   maybePlant <- findPlantWithId plantId

--   case maybePlant of
--     Just plant -> do
--       uprint $ "Искомое растение: " ++ ushow plant

--       plantContraindications <- getContraindicationsByIds $ contraindications plant
--       uprint $ "Противопоказания: " ++ ushow plantContraindications

--       plantGrowingAreas <- getAreasByIds $ growing_area plant
--       uprint $ "Ареалы произрастания: " ++ ushow plantGrowingAreas

--     Nothing -> do
--       putStrLn "Растения с таким id не существует"


findPlantWithId :: Int -> IO (Maybe Plant)
findPlantWithId plantId = do
  plantsById <- getPlantsByIds [plantId]
  return $ head plantsById


showSortedPlantsFlow :: (Plant -> Plant -> Ordering) -> [Plant] -> IO ()
showSortedPlantsFlow compareCondition plantsTreatingDisease = do
  let sortedPlants = sortBy compareCondition plantsTreatingDisease
  uprint $ ushow sortedPlants
