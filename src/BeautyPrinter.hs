module BeautyPrinter (
  printAllDiseases
  , printTreatmentCourse
  , printPlantsForDisease
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


printTreatmentCourse :: [Maybe Plants.Plant] -> Diseases.Reciept -> Float -> Int -> IO ()
printTreatmentCourse plants reciept treatmentCost treatmentDuration = do
  putStrLn "\n"
  putStrLn "Рецепт:"

  let zipPlantsReciept = zip [plant | Just plant <- plants] reciept

  let printablePlants = [printTreatmentPlantsClosure plantRecieptItem | plantRecieptItem <- zipPlantsReciept]
    in mapM_ pPrint printablePlants

  pPrint $ "Длительность курса: " ++ show treatmentDuration ++ " дней"
  pPrint $ "Стоимость: " ++ show treatmentCost ++ " усл/ед"


printPlantsForDisease :: [Plants.Plant] -> String -> IO ()
printPlantsForDisease plants diseaseName = do
  putStrLn "\n"
  putStrLn $ "Лекарственные растения для заболевания " ++ diseaseName
  putStrLn "\n"

  let printablePlants = map printPlantsForDiseaseClosure plants
    in mapM_ pPrint printablePlants


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


printTreatmentPlantsClosure :: (Plants.Plant, Diseases.RecieptItem) -> String
printTreatmentPlantsClosure originalPlantRecieptItem =
  "\n"
   ++ "id: "
   ++ show ( Plants.id $ fst originalPlantRecieptItem ) ++ "\n"
   ++ russianInterpretation (fst originalPlantRecieptItem) ++ "\n"
   ++ plantAmount originalPlantRecieptItem ++ "\n"
   ++ plantPrice (fst originalPlantRecieptItem) ++ "\n"
    where
      russianInterpretation plant = "Русское название: " ++ Plants.russian_name plant
      plantAmount plantRecieptItem = "Количество: " ++ show (Diseases.amount (snd plantRecieptItem)) ++ " пачки"
      plantPrice plant = "Цена: " ++ show (Plants.price plant) ++ " усл/ед"


printPlantsForDiseaseClosure :: Plants.Plant -> String
printPlantsForDiseaseClosure originalPlant =
  "\n"
   ++ "id: "
   ++ show ( Plants.id originalPlant ) ++ "\n"
   ++ russianInterpretation originalPlant ++ "\n"
   ++ latinInterpretation originalPlant ++ "\n"
   ++ plantPrice originalPlant ++ "\n"
    where
      russianInterpretation plant = "Русское название: " ++ Plants.russian_name plant
      latinInterpretation plant = "Латинское название: " ++ Plants.latin_name plant
      plantPrice plant = "Цена: " ++ show (Plants.price plant) ++ " усл/ед"
