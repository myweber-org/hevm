module FibonacciSequence where

fibonacci :: Int -> Integer
fibonacci n = fibs !! n
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
  putStrLn "First 20 Fibonacci numbers:"
  mapM_ (putStrLn . show . fibonacci) [0..19]module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n
    | n <= 0    = []
    | n == 1    = [0]
    | n == 2    = [0, 1]
    | otherwise = let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
                  in take n fibs

main :: IO ()
main = do
    putStrLn "Fibonacci sequence up to 10 terms:"
    print $ fibonacci 10module FibonacciSequence where

fibonacci :: Int -> [Int]
fibonacci n = takeWhile (<= n) fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
    putStrLn "Enter a number:"
    input <- getLine
    let n = read input :: Int
    putStrLn $ "Fibonacci numbers up to " ++ show n ++ ":"
    print $ fibonacci nmodule FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
  putStrLn "Fibonacci sequence up to 10 terms:"
  print $ fibonacci 10module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)