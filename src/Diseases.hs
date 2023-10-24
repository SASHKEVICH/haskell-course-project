{-# LANGUAGE DeriveGeneric #-}

module Diseases (
  showAllDiseases
) where

-- Imports

import Prelude hiding ( id )
import Data.Aeson
import Data.List ( find )
import GHC.Generics ( Generic )
import Text.Show.Unicode ( uprint )
import Text.Read
import System.Exit ( exitSuccess )
import qualified Data.ByteString.Lazy as B

import TryAgain ( tryAgain )
import Plants

-- Declarations

data RecieptItem = Reciept
  { plant_id :: Int
  , amount :: Float
  } deriving (Show, Generic)

type Reciept = [RecieptItem]

data Disease = Disease 
  { id :: Int
  , russian_name :: String
  , latin_name :: String
  , reciept :: Reciept
  , duration :: Int    
  } deriving (Show, Generic)

newtype DiseasesJSON = DiseasesJSON 
  { diseases :: [Disease]
  } deriving (Show, Generic)

instance FromJSON RecieptItem
instance FromJSON Disease
instance FromJSON DiseasesJSON

-- Functions

jsonFile :: FilePath
jsonFile = "./db/diseases.json"


getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile


getAllDiseases :: IO [Disease]
getAllDiseases = do
  decodedDiseasesJSON <- (decode <$> getJSON) :: IO (Maybe DiseasesJSON)

  case decodedDiseasesJSON of
    Just diseasesJson -> return $ diseases diseasesJson
    Nothing -> return []


showAllDiseases :: IO ()
showAllDiseases = do 
  allDiseases <- getAllDiseases
  uprint allDiseases

  mainMenu


mainMenu :: IO ()
mainMenu = do
  putStrLn "\nМеню:"
  putStrLn "1. Показать лекарственные растения"
  putStrLn "2. Вычислить стоимость курса лечения"
  putStrLn "3. Выйти в начало"
  putStrLn "4. Выйти из программы"

  decision <- getLine

  case decision of
    "1" -> showPlantsTreatingDiseaseFlow
    "2" -> calculateTreatmentCourseFlow
    "3" -> exitSuccess
    "4" -> exitSuccess
    _ -> tryAgain mainMenu


showPlantsTreatingDiseaseFlow :: IO ()
showPlantsTreatingDiseaseFlow = do
  putStrLn "Введите id заболевания:"

  decision <- getLine

  let diseaseId = readMaybe decision :: Maybe Int
  case diseaseId of
    Just diseaseId -> showPlantsTreatingDisease diseaseId

    Nothing -> do
      putStrLn "Введите число"
      calculateTreatmentCourseFlow

calculateTreatmentCourseFlow :: IO ()
calculateTreatmentCourseFlow = do
  putStrLn "Введите id заболевания:"

  decision <- getLine

  let diseaseId = readMaybe decision :: Maybe Int
  case diseaseId of
    Just diseaseId -> tryCalculateTreatmentCourseFlow diseaseId

    Nothing -> do
      putStrLn "Введите число"
      calculateTreatmentCourseFlow

tryCalculateTreatmentCourseFlow :: Int -> IO ()
tryCalculateTreatmentCourseFlow diseaseId = do
  maybeDisease <- findDiseaseWithId diseaseId

  case maybeDisease of 
    Just disease -> do
      let plantsIds = getPlantsIdsFromReciept $ reciept disease
      healthyPlants <- getPlantsByIds plantsIds

      let treatmentCourcePrice = calculateTreatmentCourse (reciept disease) healthyPlants (duration disease)

      putStrLn "Лекарственные растения:\n"
      uprint healthyPlants
      uprint $ "Стоимость: " ++ show treatmentCourcePrice ++ " усл/ед"
    
    Nothing -> do
      putStrLn "Болезни с таким id не существует"
      
  mainMenu


findDiseaseWithId :: Int -> IO (Maybe Disease)
findDiseaseWithId diseaseId = do
  allDiseases <- getAllDiseases

  let filteredDiseases = find (\disease -> id disease == diseaseId) allDiseases
  return filteredDiseases


getPlantsIdsFromReciept :: Reciept -> [Int]
getPlantsIdsFromReciept recieptArray =
  [ plant_id recieptItem | recieptItem <- recieptArray ]


calculateTreatmentCourse :: Reciept -> [Maybe Plant] -> Int -> Float
calculateTreatmentCourse recieptArray recieptPlants treatmentDuration =
  foldl (\acc x -> fromIntegral (fst x) * snd x + acc) 0.0 priceAmountArray * fromIntegral treatmentDuration
  where 
    priceAmountArray = zip [ price plant | Just plant <- recieptPlants ] [ amount recieptItem | recieptItem <- recieptArray ]
