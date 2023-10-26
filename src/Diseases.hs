{-# LANGUAGE DeriveGeneric #-}

module Diseases (
  Disease(..),
  getAllDiseases
  , calculateTreatmentCourseFlow
) where

-- Imports

import Prelude hiding ( id )
import Data.Aeson
import Data.List ( find )
import GHC.Generics ( Generic )
import Text.Show.Unicode ( uprint, ushow )
import Text.Read

import GetJSON ( decodeJson )
import Plants ( Plant(..), getPlantsByIds )

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


getAllDiseases :: IO [Disease]
getAllDiseases = do
  decodedDiseasesJSON <- decodeJson jsonFile :: IO (Maybe DiseasesJSON)

  case decodedDiseasesJSON of
    Just diseasesJson -> return $ diseases diseasesJson
    Nothing -> return []


calculateTreatmentCourseFlow :: IO ()
calculateTreatmentCourseFlow = do
  putStrLn "Введите id заболевания:"

  decision <- getLine

  let diseaseId = readMaybe decision :: Maybe Int
  case diseaseId of
    Just realDiseaseId -> tryCalculateTreatmentCourseFlow realDiseaseId

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
      uprint $ ushow healthyPlants
      uprint $ "Стоимость: " ++ show treatmentCourcePrice ++ " усл/ед"
    
    Nothing -> do
      putStrLn "Болезни с таким id не существует"


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
