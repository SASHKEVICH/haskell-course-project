module TryAgain (
  tryAgain
) where

tryAgain :: IO () -> IO ()
tryAgain function = do
  putStrLn "\nПопробуйте еще раз"
  function