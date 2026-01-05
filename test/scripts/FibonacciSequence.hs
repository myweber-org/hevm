
module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)module FibonacciSequence where

fibonacci :: Integer -> [Integer]
fibonacci n = takeWhile (<= n) fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
  putStrLn "Enter a number:"
  input <- getLine
  let n = read input :: Integer
  putStrLn $ "Fibonacci sequence up to " ++ show n ++ ":"
  print $ fibonacci n