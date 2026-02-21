module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

printFibonacci :: Int -> IO ()
printFibonacci n = do
    putStrLn $ "First " ++ show n ++ " Fibonacci numbers:"
    mapM_ (putStrLn . show) (fibonacci n)

main :: IO ()
main = do
    putStr "Enter number of terms: "
    input <- getLine
    let n = read input :: Int
    if n > 0
        then printFibonacci n
        else putStrLn "Please enter a positive integer."module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
  putStrLn "Fibonacci sequence up to 10 terms:"
  print $ fibonacci 10