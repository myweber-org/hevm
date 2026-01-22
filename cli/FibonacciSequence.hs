module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n
    | n <= 0    = []
    | n == 1    = [0]
    | n == 2    = [0, 1]
    | otherwise = let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
                  in take n fibs

printFibonacci :: Int -> IO ()
printFibonacci n = do
    putStrLn $ "Fibonacci sequence up to " ++ show n ++ " terms:"
    mapM_ (putStrLn . show) (fibonacci n)