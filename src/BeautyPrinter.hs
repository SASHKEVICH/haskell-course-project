module BeautyPrinter (
  printAllDiseases
) where

-- Imports 

import Prelude hiding ( id )
import Text.Pretty.Simple ( pPrint )

import Diseases ( Disease(..) )

-- Functions

printAllDiseases :: [Disease] -> IO ()
printAllDiseases diseases = do
  let printableDiseases = map printDiseasesClosure diseases
    in mapM_ pPrint printableDiseases


printDiseasesClosure :: Disease -> String
printDiseasesClosure originalDisease =
  "\n"
   ++ "id: " 
   ++ show ( id originalDisease ) ++ "\n"
   ++ russianInterpretation originalDisease ++ "\n"
   ++ latinInterpretation originalDisease ++ "\n"
    where
      russianInterpretation disease = "Русское название: " ++ russian_name disease
      latinInterpretation disease = "Латинское название: " ++ latin_name disease
      