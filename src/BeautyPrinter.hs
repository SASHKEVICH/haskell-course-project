module BeautyPrinter (
  printAllDiseases
  , printTreatmentCourse
) where

-- Imports 

import Prelude hiding ( id )
import Text.Pretty.Simple ( pPrint )

import qualified Diseases
import qualified Plants

-- Functions

printAllDiseases :: [Diseases.Disease] -> IO ()
printAllDiseases diseases = do
  let printableDiseases = map printDiseasesClosure diseases
    in mapM_ pPrint printableDiseases


printTreatmentCourse :: [Maybe Plants.Plant] -> Float -> Int -> IO ()
printTreatmentCourse plants treatmentCost treatmentDuration = do
  putStrLn "Лекарственные растения:\n"

  let printablePlants = [printTreatmentPlantsClosure plant | Just plant <- plants]
    in mapM_ pPrint printablePlants

  putStrLn $ "Длительность курса: " ++ show treatmentDuration ++ " дней"
  putStrLn $ "Стоимость: " ++ show treatmentCost ++ " усл/ед"


printDiseasesClosure :: Diseases.Disease -> String
printDiseasesClosure originalDisease =
  "\n"
   ++ "id: "
   ++ show ( Diseases.id originalDisease ) ++ "\n"
   ++ russianInterpretation originalDisease ++ "\n"
   ++ latinInterpretation originalDisease ++ "\n"
    where
      russianInterpretation disease = "Русское название: " ++ Diseases.russian_name disease
      latinInterpretation disease = "Латинское название: " ++ Diseases.latin_name disease


printTreatmentPlantsClosure :: Plants.Plant -> String
printTreatmentPlantsClosure originalPlant =
  "\n"
   ++ "id: "
   ++ show ( Plants.id originalPlant ) ++ "\n"
   ++ russianInterpretation originalPlant ++ "\n"
   ++ plantPrice originalPlant ++ "\n"
    where
      russianInterpretation plant = "Русское название: " ++ Plants.russian_name plant
      plantPrice plant = "Цена: " ++ show (Plants.price plant) ++ " усл/ед"
