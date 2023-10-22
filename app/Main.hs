module Main (main) where

import Diseases

import Data.Aeson
import Text.Show.Unicode (uprint)

main :: IO ()
main = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String DiseasesJSON)

  case d of
    Left err -> putStrLn err
    Right ps -> uprint ps
