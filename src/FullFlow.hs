module FullFlow (
  mainFlow
) where

-- Imports

import System.Exit ( exitSuccess )
import Text.Show.Unicode
import Text.Read

import Diseases ( getAllDiseases, calculateTreatmentCourseFlow )

import Plants
  ( Plant(..),
    getAllPlants,
    findAllPlantsTreatingDisease,
    showInformationAboutPlantFlow,
    showSortedPlantsFlow )

import TryAgain ( tryAgain )

-- Functions

mainFlow :: IO ()
mainFlow = do
  putStrLn "\n"
  putStrLn "Программа \"Аптека лекарственных растений\""

  mainMenu


mainMenu :: IO ()
mainMenu = do
  putStrLn "\n"
  putStrLn "Меню:"
  putStrLn "1. Показать заболевания"
  putStrLn "2. Выход из программы"

  decision <- getLine

  case decision of
    "1" -> showAllDiseasesFlow
    "2" -> exitSuccess
    _ -> tryAgain mainMenu


showAllDiseasesFlow :: IO ()
showAllDiseasesFlow = do 
  allDiseases <- getAllDiseases
  uprint allDiseases

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
  putStrLn "\nМеню:"
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
      showSortedPlantsFlow compareCondition plantsTreatingDisease
      plantsMenu plantsTreatingDisease

    "3" -> do
      let compareCondition lt rt = if price lt > price rt then GT else LT
      showSortedPlantsFlow compareCondition plantsTreatingDisease
      plantsMenu plantsTreatingDisease

    "4" -> showAllDiseasesFlow
    "5" -> mainFlow
    "6" -> exitSuccess
    _ -> tryAgain $ plantsMenu plantsTreatingDisease


showPlantsTreatingDiseaseFlow :: Int -> IO ()
showPlantsTreatingDiseaseFlow diseaseId = do
  allPlants <- getAllPlants

  let plantsTreatingDisease = findAllPlantsTreatingDisease allPlants diseaseId
  uprint plantsTreatingDisease

  plantsMenu plantsTreatingDisease