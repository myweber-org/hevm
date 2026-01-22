module FibonacciSequence where

import Data.Function (fix)

fibonacci :: Int -> Integer
fibonacci n = fibList !! n
  where
    fibList = 0 : 1 : zipWith (+) fibList (tail fibList)

-- Alternative implementation using fix for demonstration
fibonacciFix :: Int -> Integer
fibonacciFix = fix (\rec n -> if n < 2 then fromIntegral n else rec (n-1) + rec (n-2))