module FibonacciSequence where

import Data.Function (fix)

fibonacci :: Int -> Integer
fibonacci n = fibList !! n
  where
    fibList = 0 : 1 : zipWith (+) fibList (tail fibList)

-- Alternative implementation using fix for memoization
fibonacci' :: Int -> Integer
fibonacci' = fix (\rec n -> if n < 2 then fromIntegral n else rec (n-1) + rec (n-2))

-- Fast doubling algorithm for O(log n) Fibonacci
fibFastDoubling :: Int -> Integer
fibFastDoubling n
    | n < 0     = error "Negative input not supported"
    | otherwise = fst (fibPair n)
  where
    fibPair 0 = (0, 1)
    fibPair k
        | even k    = (a * (2*b - a), a*a + b*b)
        | otherwise = (a*a + b*b, b * (2*a + b))
      where
        (a, b) = fibPair (k `div` 2)