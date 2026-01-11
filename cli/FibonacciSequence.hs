module FibonacciSequence where

fibonacci :: Integer -> [Integer]
fibonacci n = takeWhile (<= n) fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)module Fibonacci where

fibonacci :: Int -> Integer
fibonacci n = fibs !! n
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
  putStrLn "First 20 Fibonacci numbers:"
  mapM_ (print . fibonacci) [0..19]