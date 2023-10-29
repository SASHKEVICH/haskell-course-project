module ReadDecision (
  readDecision
) where

-- Imports

import System.IO ( hFlush, stdout )

-- Functions

readDecision :: IO String
readDecision = do
  hFlush stdout
  getLine
