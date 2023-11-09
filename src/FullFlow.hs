module FullFlow (
  mainFlow
) where

-- Imports

import System.Exit ( exitSuccess )
import Text.Read ( readMaybe )
import System.Console.ANSI ( clearScreen )
import Data.List ( sortBy )

import Diseases
  ( Disease( reciept, duration, russian_name ),
    getAllDiseases,
    findDiseaseWithId,
    getPlantsIdsFromReciept,
    calculateTreatmentCourse )
import Plants
  ( Plant(..),
    getAllPlants,
    findAllPlantsTreatingDisease,
    getPlantsByIds,
    findPlantWithId )
import TryAgain ( tryAgain )
import BeautyPrinter
  ( printAllDiseases,
    printTreatmentCourse,
    printPlantsForDisease,
    printAdditionalInfoAboutPlant, 
    printSortedPlants,
    printSortedPlantsWithGrowingAreas )
import Contraindications ( getContraindicationsByIds )
import GrowingAreas ( GrowingArea (russian_name), getAreasByIds, getAllAreas, findAreasByIds )
import ReadDecision ( readDecision )

-- Functions

mainFlow :: IO ()
mainFlow = do
  clearScreen

  putStrLn "\n"
  putStrLn "Программа \"Аптека лекарственных растений\""

  mainMenu


mainMenu :: IO ()
mainMenu = do
  putStrLn "\n"
  putStrLn "Меню:"
  putStrLn "1. Показать заболевания"
  putStrLn "2. Выход из программы"

  decision <- readDecision

  case decision of
    "1" -> showAllDiseasesFlow
    "2" -> exitSuccess
    _ -> tryAgain mainMenu


showAllDiseasesFlow :: IO ()
showAllDiseasesFlow = do
  clearScreen

  putStrLn "Список заболеваний:\n"

  allDiseases <- getAllDiseases
  printAllDiseases allDiseases

  diseasesMenu


diseasesMenu :: IO ()
diseasesMenu = do
  putStrLn "\nМеню:"
  putStrLn "1. Показать лекарственные растения для заболевания"
  putStrLn "2. Показать все заболевания"
  putStrLn "3. Вычислить стоимость курса лечения"
  putStrLn "4. Выйти в начало"
  putStrLn "5. Выйти из программы"

  decision <- readDecision

  case decision of
    "1" -> requestShowPlantsTreatingDiseaseFlow
    "2" -> showAllDiseasesFlow
    "3" -> do
      calculateTreatmentCourseFlow
      diseasesMenu
    "4" -> mainFlow
    "5" -> exitSuccess
    _ -> tryAgain mainMenu


calculateTreatmentCourseFlow :: IO ()
calculateTreatmentCourseFlow = do
  putStrLn "Введите id заболевания:"

  decision <- readDecision

  let diseaseId = readMaybe decision :: Maybe Int
  case diseaseId of
    Just realDiseaseId -> tryCalculateTreatmentCourseFlow realDiseaseId

    Nothing -> do
      putStrLn "Введите число"
      calculateTreatmentCourseFlow


tryCalculateTreatmentCourseFlow :: Int -> IO ()
tryCalculateTreatmentCourseFlow diseaseId = do
  maybeDisease <- findDiseaseWithId diseaseId

  case maybeDisease of
    Just disease -> do
      let plantsIds = getPlantsIdsFromReciept $ reciept disease
      healthyPlants <- getPlantsByIds plantsIds

      let treatmentCourseCost = calculateTreatmentCourse (reciept disease) healthyPlants (duration disease)
      printTreatmentCourse healthyPlants (reciept disease) treatmentCourseCost (duration disease)

    Nothing -> do
      putStrLn "Болезни с таким id не существует"


requestShowPlantsTreatingDiseaseFlow :: IO ()
requestShowPlantsTreatingDiseaseFlow = do
  putStrLn "Введите id заболевания:"

  decision <- readDecision

  let diseaseId = readMaybe decision :: Maybe Int
  case diseaseId of
    Just realDiseaseId -> showPlantsTreatingDiseaseFlow realDiseaseId

    Nothing -> do
      putStrLn "Введите число"
      requestShowPlantsTreatingDiseaseFlow


plantsMenu :: [Plant] -> IO ()
plantsMenu plantsTreatingDisease = do
  putStrLn "\n"
  putStrLn "Меню:"
  putStrLn "1. Показать полную информацию о растении"
  putStrLn "2. Отсортировать растения по цене"
  putStrLn "3. Отсортировать растения по ареалу произрастания"
  putStrLn "4. Назад"
  putStrLn "5. Выйти в начало"
  putStrLn "6. Выйти из программы"

  decision <- readDecision

  case decision of
    "1" -> do
      showInformationAboutPlantFlow plantsTreatingDisease
      plantsMenu plantsTreatingDisease

    "2" -> do
      let compareCondition lt rt = if price lt > price rt then GT else LT
      showSortedPlantsFlow compareCondition plantsTreatingDisease "Сортировка по цене"
      plantsMenu plantsTreatingDisease

    "3" -> do
      sortByGrowingAreaFlow plantsTreatingDisease
      plantsMenu plantsTreatingDisease

    "4" -> showAllDiseasesFlow
    "5" -> mainFlow
    "6" -> exitSuccess
    _ -> tryAgain $ plantsMenu plantsTreatingDisease


showPlantsTreatingDiseaseFlow :: Int -> IO ()
showPlantsTreatingDiseaseFlow diseaseId = do
  allPlants <- getAllPlants

  disease <- findDiseaseWithId diseaseId

  case disease of
    Just realDisease -> do
      let plantsTreatingDisease = findAllPlantsTreatingDisease allPlants diseaseId
      printPlantsForDisease plantsTreatingDisease (Diseases.russian_name realDisease)

      plantsMenu plantsTreatingDisease

    Nothing -> do
      putStrLn "Такой болезни не найдено"
      plantsMenu []


showInformationAboutPlantFlow :: [Plant] -> IO ()
showInformationAboutPlantFlow plantsTreatingDisease = do
  putStrLn "Введите id растения:"

  decision <- readDecision

  let plantId = readMaybe decision :: Maybe Int
  case plantId of
    Just realPlantId -> tryShowInformationAboutPlantFlow realPlantId
    Nothing -> do
      putStrLn "Введите число"
      showInformationAboutPlantFlow plantsTreatingDisease


tryShowInformationAboutPlantFlow :: Int -> IO ()
tryShowInformationAboutPlantFlow plantId = do
  maybePlant <- findPlantWithId plantId

  case maybePlant of
    Just plant -> do
      clearScreen

      plantContraindications <- getContraindicationsByIds $ contraindications plant
      plantGrowingAreas <- getAreasByIds $ growing_area plant
      printAdditionalInfoAboutPlant
        plant
        plantContraindications
        plantGrowingAreas

    Nothing -> do
      putStrLn "Растения с таким id не существует"
      plantsMenu []


showSortedPlantsFlow :: (Plant -> Plant -> Ordering) -> [Plant] -> String -> IO ()
showSortedPlantsFlow compareCondition plantsTreatingDisease message = do
  let sortedPlants = sortBy compareCondition plantsTreatingDisease
  printSortedPlants sortedPlants message


sortByGrowingAreaFlow :: [Plant] -> IO()
sortByGrowingAreaFlow plants = do
  putStrLn "Выберите способ сортировки по ареалу:"
  putStrLn "1. По первому ареалу произрастания растения"
  putStrLn "2. По количеству ареалов произрастания растения"

  decision <- readDecision

  areasWithEachPlant <- zipGrowingAreasWithEachPlant plants

  case decision of
    "1" -> do
      let sortedPlants = sortBy sortPlantsByFirstAreaCondition areasWithEachPlant
      printSortedPlantsWithGrowingAreas sortedPlants "---Сортировка по первому ареалу"

    "2" -> do
      let compareCondition lt rt = if length (growing_area $ fst lt) > length (growing_area $ fst rt) then GT else LT
      let sortedPlants = sortBy compareCondition areasWithEachPlant
      printSortedPlantsWithGrowingAreas sortedPlants "---Сортировка по количеству ареалов"

    _ -> plantsMenu plants


zipGrowingAreasWithEachPlant:: [Plant] -> IO [(Plant, [GrowingArea])]
zipGrowingAreasWithEachPlant plants = do
  allAreas <- getAllAreas
  let areasGroupedByPlants = [ findAreasByIds allAreas (growing_area plant) | plant <- plants ]
  return $ zip plants areasGroupedByPlants


sortPlantsByFirstAreaCondition :: (Plant, [GrowingArea]) -> (Plant, [GrowingArea]) -> Ordering
sortPlantsByFirstAreaCondition lt rt
  | char lt > char rt = GT
  | otherwise = LT
  where
    char element = head $ GrowingAreas.russian_name (head (snd element))
