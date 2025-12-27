module FibonacciSequence where

fib :: Int -> Integer
fib n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)module FibonacciSequence where

import Data.Function (fix)

fibonacci :: Int -> Integer
fibonacci n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Alternative implementation using fix for memoization
fibonacci' :: Int -> Integer
fibonacci' = fix (\rec n -> if n < 2 then fromIntegral n else rec (n-1) + rec (n-2))