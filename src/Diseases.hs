{-# LANGUAGE DeriveGeneric #-}

module Diseases (
  showAllDiseases
) where

-- Imports

import Prelude hiding (id) 
import Data.Aeson
import GHC.Generics
import Text.Show.Unicode (uprint)
import Text.Read
import System.Exit (exitSuccess)
import qualified Data.ByteString.Lazy as B

import TryAgain (tryAgain)

-- Declarations

data Reciept = Reciept
  { plant_id :: Int
  , amount :: Float
  } deriving (Show, Generic)

data Disease = Disease 
  { id :: Int
  , russian_name :: String
  , latin_name :: String
  , reciept :: [Reciept]
  , duration :: Int    
  } deriving (Show, Generic)

newtype DiseasesJSON = DiseasesJSON 
  { diseases :: [Disease]
  } deriving (Show, Generic)

instance FromJSON Reciept
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
  putStrLn "1. Показать заболевания"
  putStrLn "2. Вычислить стоимость курса лечения"
  putStrLn "3. Выйти в начало"
  putStrLn "4. Выйти из программы"

  decision <- getLine

  case decision of
    "1" -> exitSuccess
    "2" -> calculateTreatmentCourseFlow
    "3" -> exitSuccess
    "4" -> exitSuccess
    _ -> tryAgain mainMenu


calculateTreatmentCourseFlow :: IO ()
calculateTreatmentCourseFlow = do
  putStrLn "Введите id заболевания:"

  decision <- getLine

  let disease_id = readMaybe decision :: Maybe Int
  case disease_id of
    Just just_disease_id -> tryCalculateTreatmentCourseFlow just_disease_id

    Nothing -> do
      putStrLn "Введите число"
      calculateTreatmentCourseFlow

tryCalculateTreatmentCourseFlow :: Int -> IO ()
tryCalculateTreatmentCourseFlow disease_id = do
  maybeDisease <- findDiseaseWithId disease_id

  case maybeDisease of 
    Just disease -> do
      uprint disease
    
    Nothing -> do
      putStrLn "Болезни с таким id не существует"
      
  mainMenu


findDiseaseWithId :: Int -> IO (Maybe Disease)
findDiseaseWithId disease_id = do
  allDiseases <- getAllDiseases

  let filteredDiseases = filterDiseasesWithId allDiseases disease_id
  return filteredDiseases


filterDiseasesWithId :: [Disease] -> Int -> Maybe Disease
filterDiseasesWithId allDiseases disease_id
  | null filteredDiseases = Nothing
  | otherwise = Just (head filteredDiseases)
  where 
    filteredDiseases = filter (\disease -> id disease == disease_id) allDiseases
