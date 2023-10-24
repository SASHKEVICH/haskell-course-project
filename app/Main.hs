module Main (main) where

import System.Exit (exitSuccess)

import Diseases (showAllDiseases)
import TryAgain (tryAgain)

main :: IO ()
main = do
  putStrLn "\nПрограмма \"Аптека лекарственных растений\""
  menu

menu :: IO ()
menu = do
  putStrLn "\nМеню:"
  putStrLn "1. Показать заболевания"
  putStrLn "2. Выход из программы"

  decision <- getLine

  case decision of
    "1" -> showAllDiseases
    "2" -> exitSuccess
    _ -> tryAgain menu
