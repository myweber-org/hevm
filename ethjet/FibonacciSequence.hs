module FibonacciSequence where

import Data.Function (fix)

fibonacci :: Int -> Integer
fibonacci n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibonacciMemoized :: Int -> Integer
fibonacciMemoized = fix (\rec n -> if n < 2 then fromIntegral n else rec (n - 1) + rec (n - 2))