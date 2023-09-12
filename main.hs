doubleMe :: Num a => a -> a
doubleMe x = x + x

main :: IO ()
main = do 
    putStrLn ("--- " ++ show ( doubleMe 2 ) ++ " ---")