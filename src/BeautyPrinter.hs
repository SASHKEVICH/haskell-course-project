module BeautyPrinter (
  printAllDiseases
  , printTreatmentCourse
  , printPlantsForDisease
  , printAdditionalInfoAboutPlant
) where

-- Imports 

import Prelude hiding ( id )
import Text.Pretty.Simple ( pPrint )
import Text.Show.Unicode ( ushow )

import qualified Diseases
import qualified Plants
import qualified Contraindications
import qualified GrowingAreas

-- Functions

printAllDiseases :: [Diseases.Disease] -> IO ()
printAllDiseases diseases = do printStuff printDiseasesClosure diseases


printTreatmentCourse :: [Maybe Plants.Plant] -> Diseases.Reciept -> Float -> Int -> IO ()
printTreatmentCourse plants reciept treatmentCost treatmentDuration = do
  putStrLn "\n"
  putStrLn "---Рецепт:"

  let zipPlantsReciept = Prelude.zip [plant | Just plant <- plants] reciept

  printStuff
    printTreatmentPlantsClosure
    zipPlantsReciept

  pPrint $ "Длительность курса: " ++ show treatmentDuration ++ " дней"
  pPrint $ "Стоимость: " ++ show treatmentCost ++ " усл/ед"


printPlantsForDisease :: [Plants.Plant] -> String -> IO ()
printPlantsForDisease plants diseaseName = do
  putStrLn "\n"
  putStrLn $ "---Лекарственные растения для заболевания " ++ diseaseName

  let printablePlants = Prelude.map printPlantsForDiseaseClosure plants
    in mapM_ pPrint printablePlants


printAdditionalInfoAboutPlant
  :: Plants.Plant -> [Contraindications.Contraindication] -> [GrowingAreas.GrowingArea] -> IO ()
printAdditionalInfoAboutPlant plant contraindications growingAreas = do
  putStrLn "\n"
  putStrLn $ "---Полная информация о растении " ++ Plants.russian_name plant

  let printablePlant = printAdditionalInfoAboutPlantClosure plant
    in pPrint printablePlant

  putStrLn "---Противопоказания:"

  printStuff printContraindicationsClosure contraindications

  putStrLn "---Ареалы произрастания:"
  printStuff printGrowingAreasClosure growingAreas

  putStrLn ""


printDiseasesClosure :: Diseases.Disease -> String
printDiseasesClosure originalDisease =
  id Diseases.id originalDisease ++ "\n"
  ++ russianInterpretation Diseases.russian_name originalDisease ++ "\n"
  ++ latinInterpretation Diseases.latin_name originalDisease ++ "\n"


printTreatmentPlantsClosure :: (Plants.Plant, Diseases.RecieptItem) -> String
printTreatmentPlantsClosure originalPlantRecieptItem =
  id Plants.id (fst originalPlantRecieptItem) ++ "\n"
  ++ russianInterpretation Plants.russian_name (fst originalPlantRecieptItem) ++ "\n"
  ++ plantAmount originalPlantRecieptItem ++ "\n"
  ++ plantPrice (fst originalPlantRecieptItem) ++ "\n"
  where
    plantAmount plantRecieptItem = "Количество: " ++ show (Diseases.amount (snd plantRecieptItem)) ++ " пачки"
    plantPrice plant = "Цена: " ++ show (Plants.price plant) ++ " усл/ед"


printPlantsForDiseaseClosure :: Plants.Plant -> String
printPlantsForDiseaseClosure originalPlant =
  id Plants.id originalPlant ++ "\n"
  ++ russianInterpretation Plants.russian_name originalPlant ++ "\n"
  ++ latinInterpretation Plants.latin_name originalPlant ++ "\n"
  ++ plantPrice originalPlant ++ "\n"
    where
      plantPrice plant = "Цена: " ++ show (Plants.price plant) ++ " усл/ед"


printAdditionalInfoAboutPlantClosure :: Plants.Plant -> String
printAdditionalInfoAboutPlantClosure originalPlant =
  id Plants.id originalPlant ++ "\n"
  ++ russianInterpretation Plants.russian_name originalPlant ++ "\n"
  ++ latinInterpretation Plants.latin_name originalPlant ++ "\n"
  ++ plantPrice originalPlant ++ "\n\n"
  ++ additionalInfo originalPlant ++ "\n"
    where
      plantPrice plant = "Цена: " ++ show (Plants.price plant) ++ " усл/ед"
      additionalInfo plant = "Дополнительная информация:\n" ++ Plants.information plant


printContraindicationsClosure :: Contraindications.Contraindication -> String
printContraindicationsClosure originalContraindication =
  id Contraindications.id originalContraindication
  ++ "\n"
  ++ russianInterpretation Contraindications.russian_name originalContraindication
  ++ "\n"


printGrowingAreasClosure :: GrowingAreas.GrowingArea -> String
printGrowingAreasClosure originalGrowingArea =
  id GrowingAreas.id originalGrowingArea ++ "\n"
  ++ russianInterpretation GrowingAreas.russian_name originalGrowingArea ++ "\n"
  ++ "Локация: " ++ GrowingAreas.location originalGrowingArea


printStuff :: (a -> String) -> [a] -> IO ()
printStuff closure originalStuff = do
  let printableStuff = Prelude.map closure originalStuff
    in mapM_ pPrint printableStuff


id :: (Show b) => (a -> b) -> a -> String
id closure object =
  "\n" ++ "id: " ++ ushow (closure object)


russianInterpretation :: (Show b) => (a -> b) -> a -> String
russianInterpretation closure object =
  "Русское название: " ++ ushow (closure object)


latinInterpretation :: (Show b) => (a -> b) -> a -> String
latinInterpretation closure object =
  "Латинское название: " ++ ushow (closure object)