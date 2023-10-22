module Main where

import qualified MyLib (someFunc)
import Plants

foo :: Plant -> String
foo plant = show plant

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"

  print $ foo Plant { 
    plant_id = 1, 
    russian_name = "Something", 
    latin_name = "",
    price = 123,
    information = "",
    growing_area = [],
    contraindications = [],
    diseases = []
    }
