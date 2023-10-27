module FullFlow (
  mainFlow
) where

-- Imports

import System.IO ( hFlush, stdout )
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
    printAdditionalInfoAboutPlant, printSortedPlants )
import Contraindications ( getContraindicationsByIds )
import GrowingAreas ( getAreasByIds )

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

  hFlush stdout
  decision <- getLine

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

  decision <- getLine

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

  hFlush stdout
  decision <- getLine

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

  decision <- getLine

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

  decision <- getLine

  case decision of
    "1" -> do
      showInformationAboutPlantFlow plantsTreatingDisease
      plantsMenu plantsTreatingDisease

    "2" -> do
      let compareCondition lt rt = if price lt > price rt then GT else LT
      showSortedPlantsFlow compareCondition plantsTreatingDisease "Сортировка по цене"
      plantsMenu plantsTreatingDisease

    "3" -> do
      let compareCondition lt rt = if price lt > price rt then GT else LT
      showSortedPlantsFlow compareCondition plantsTreatingDisease "Сортировка по ареалу"
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
  putStrLn "Введите id растения:\n"

  decision <- getLine

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
        [contras | Just contras <- plantContraindications]
        [areas | Just areas <- plantGrowingAreas]

    Nothing -> do
      putStrLn "Растения с таким id не существует"
      plantsMenu []


showSortedPlantsFlow :: (Plant -> Plant -> Ordering) -> [Plant] -> String -> IO ()
showSortedPlantsFlow compareCondition plantsTreatingDisease message = do
  let sortedPlants = sortBy compareCondition plantsTreatingDisease
  printSortedPlants sortedPlants message
