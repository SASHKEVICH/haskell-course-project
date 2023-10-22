module Plants (
    
) where

data Plant = Plant { 
    plant_id :: Int, 
    latin_name :: String,
    russian_name :: String,
    price :: Int, 
    information :: String,
    growing_area :: [Int],
    contraindications :: [Int],
    diseases :: [Int]
    } deriving Show