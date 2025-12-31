module FibonacciSequence where

fibonacci :: Integer -> [Integer]
fibonacci n
    | n <= 0    = []
    | otherwise = takeWhile (<= n) fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)