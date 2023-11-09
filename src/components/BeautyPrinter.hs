module BeautyPrinter (
  printAllDiseases
  , printTreatmentCourse
  , printPlantsForDisease
  , printAdditionalInfoAboutPlant
  , printSortedPlants
  , printSortedPlantsWithGrowingAreas
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
printAllDiseases diseases = do printStuff printDiseasesTemplate diseases


printTreatmentCourse :: [Maybe Plants.Plant] -> Diseases.Reciept -> Float -> Int -> IO ()
printTreatmentCourse plants reciept treatmentCost treatmentDuration = do
  putStrLn "\n"
  putStrLn "---Рецепт:"

  let zipPlantsReciept = zip [plant | Just plant <- plants] reciept

  printStuff
    printTreatmentPlantsTemplate
    zipPlantsReciept

  pPrint $ "Длительность курса: " ++ show treatmentDuration ++ " дней"
  pPrint $ "Стоимость: " ++ show treatmentCost ++ " усл/ед"


printPlants :: [Plants.Plant] -> IO ()
printPlants plants = do
  let printablePlants = map printPlantsForDiseaseTemplate plants
    in mapM_ pPrint printablePlants


printPlantsForDisease :: [Plants.Plant] -> String -> IO ()
printPlantsForDisease plants diseaseName = do
  putStrLn "\n"
  putStrLn $ "---Лекарственные растения для заболевания " ++ diseaseName

  printPlants plants


printAdditionalInfoAboutPlant
  :: Plants.Plant -> [Contraindications.Contraindication] -> [GrowingAreas.GrowingArea] -> IO ()
printAdditionalInfoAboutPlant plant contraindications growingAreas = do
  putStrLn "\n"
  putStrLn $ "---Полная информация о растении " ++ Plants.russian_name plant

  let printablePlant = printAdditionalInfoAboutPlantTemplate plant
    in pPrint printablePlant

  putStrLn "---Противопоказания:"

  printStuff printContraindicationsTemplate contraindications

  putStrLn "---Ареалы произрастания:"
  printStuff printGrowingAreasTemplate growingAreas


printSortedPlants :: [Plants.Plant] -> String -> IO ()
printSortedPlants plants message = do
  putStrLn "\n"
  putStrLn message
  printPlants plants


printSortedPlantsWithGrowingAreas :: [(Plants.Plant, [GrowingAreas.GrowingArea])] -> String -> IO ()
printSortedPlantsWithGrowingAreas plantsWithGrowingAreas message = do
  putStrLn "\n"
  putStrLn message
  printStuff printSortedPlantWithGrowingAreasTemplate plantsWithGrowingAreas


printDiseasesTemplate :: Diseases.Disease -> String
printDiseasesTemplate originalDisease =
  id Diseases.id originalDisease ++ "\n"
  ++ russianInterpretation Diseases.russian_name originalDisease ++ "\n"
  ++ latinInterpretation Diseases.latin_name originalDisease ++ "\n"


printTreatmentPlantsTemplate :: (Plants.Plant, Diseases.RecieptItem) -> String
printTreatmentPlantsTemplate originalPlantRecieptItem =
  id Plants.id (fst originalPlantRecieptItem) ++ "\n"
  ++ russianInterpretation Plants.russian_name (fst originalPlantRecieptItem) ++ "\n"
  ++ plantAmount originalPlantRecieptItem ++ "\n"
  ++ plantPrice (fst originalPlantRecieptItem) ++ "\n"
  where
    plantAmount plantRecieptItem = "Количество: " ++ show (Diseases.amount (snd plantRecieptItem)) ++ " пачки"
    plantPrice plant = "Цена: " ++ show (Plants.price plant) ++ " усл/ед"


printPlantsForDiseaseTemplate :: Plants.Plant -> String
printPlantsForDiseaseTemplate originalPlant =
  id Plants.id originalPlant ++ "\n"
  ++ russianInterpretation Plants.russian_name originalPlant ++ "\n"
  ++ latinInterpretation Plants.latin_name originalPlant ++ "\n"
  ++ plantPrice originalPlant ++ "\n"
    where
      plantPrice plant = "Цена: " ++ show (Plants.price plant) ++ " усл/ед"


printAdditionalInfoAboutPlantTemplate :: Plants.Plant -> String
printAdditionalInfoAboutPlantTemplate originalPlant =
  id Plants.id originalPlant ++ "\n"
  ++ russianInterpretation Plants.russian_name originalPlant ++ "\n"
  ++ latinInterpretation Plants.latin_name originalPlant ++ "\n"
  ++ plantPrice originalPlant ++ "\n\n"
  ++ additionalInfo originalPlant ++ "\n"
    where
      plantPrice plant = "Цена: " ++ show (Plants.price plant) ++ " усл/ед"
      additionalInfo plant = "Дополнительная информация:\n" ++ Plants.information plant


printContraindicationsTemplate :: Contraindications.Contraindication -> String
printContraindicationsTemplate originalContraindication =
  id Contraindications.id originalContraindication
  ++ "\n"
  ++ russianInterpretation Contraindications.russian_name originalContraindication
  ++ "\n"


printGrowingAreasTemplate :: GrowingAreas.GrowingArea -> String
printGrowingAreasTemplate originalGrowingArea =
  id GrowingAreas.id originalGrowingArea ++ "\n"
  ++ russianInterpretation GrowingAreas.russian_name originalGrowingArea ++ "\n"
  ++ "Локация: " ++ GrowingAreas.location originalGrowingArea


printSortedPlantWithGrowingAreasTemplate :: (Plants.Plant, [GrowingAreas.GrowingArea]) -> String
printSortedPlantWithGrowingAreasTemplate plantWithAreasItem =
  printPlantsForDiseaseTemplate (fst plantWithAreasItem)
  ++ "\n"
  ++ "Ареалы произрастания:"
  ++ foldl (\acc grarea -> acc ++ printGrowingAreasTemplate grarea) "" (snd plantWithAreasItem)
  ++ "\n------------------------------------------------\n"


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