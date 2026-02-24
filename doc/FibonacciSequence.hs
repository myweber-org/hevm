module FibonacciSequence where

import Data.Function (fix)

fibonacci :: Int -> Integer
fibonacci n = fibList !! n
  where
    fibList = map fib [0..]
    fib 0 = 0
    fib 1 = 1
    fib x = fibList !! (x - 1) + fibList !! (x - 2)

-- Alternative implementation using fix for memoization
fibonacci' :: Int -> Integer
fibonacci' = (map fib [0..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibonacci' (n - 1) + fibonacci' (n - 2)

-- Fast doubling algorithm for O(log n) Fibonacci
fastFibonacci :: Int -> Integer
fastFibonacci n
    | n >= 0 = fst (fibPair n)
    | otherwise = error "Negative input not supported"
  where
    fibPair 0 = (0, 1)
    fibPair n
        | even n = (a * (2 * b - a), a * a + b * b)
        | otherwise = (a * a + b * b, b * (2 * a + b))
      where
        (a, b) = fibPair (n `div` 2)