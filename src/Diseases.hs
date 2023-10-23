{-# LANGUAGE DeriveGeneric #-}

module Diseases (
  showAllDiseases
) where

-- Imports

import Data.Aeson
import GHC.Generics
import Text.Show.Unicode (uprint)
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

showAllDiseases :: IO ()
showAllDiseases = do 
  d <- (eitherDecode <$> getJSON) :: IO (Either String DiseasesJSON)

  case d of
    Left err -> putStrLn err
    Right ps -> uprint $ diseases ps

  menu

menu :: IO ()
menu = do
  putStrLn "\nМеню:"
  putStrLn "1. Показать заболевания"
  putStrLn "2. Выйти в начало"
  putStrLn "3. Выйти из программы"

  decision <- getLine

  case decision of
    "1" -> exitSuccess
    "2" -> exitSuccess
    "3" -> exitSuccess
    _ -> tryAgain menu
