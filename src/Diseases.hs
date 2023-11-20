{-# LANGUAGE DeriveGeneric #-}

module Diseases (
  Disease(..)
  , RecieptItem (..)
  , Reciept
  , getAllDiseases
  , findDiseaseWithId
  , getPlantsIdsFromReciept
  , calculateTreatmentCourse
) where

-- Imports

import Prelude hiding ( id )
import Data.Aeson ( FromJSON )
import Data.List ( find )
import GHC.Generics ( Generic )

import GetJSON ( decodeJson )
import Plants ( Plant( price ) )

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
  foldl (\acc x -> fromIntegral (fst x) * fromIntegral (snd x) + acc) 0.0 priceAndPackAmountArray
  where
    priceAndPackAmountArray :: [(Int, Int)]
    priceAndPackAmountArray =
      zip
      [ price plant | Just plant <- recieptPlants ]
      [ wholePacksAmount recieptItem | recieptItem <- recieptArray ]
    wholePacksAmount recieptItem = ceiling $ amount recieptItem * fromIntegral treatmentDuration
