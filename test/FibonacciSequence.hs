module FibonacciSequence where

import Data.Function (fix)

fibonacci :: Int -> Integer
fibonacci = (map fib [0..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibonacci (n - 1) + fibonacci (n - 2)

-- Alternative implementation using fix for memoization
fibonacciMemoized :: Int -> Integer
fibonacciMemoized = fix memoFib
  where
    memoFib _ 0 = 0
    memoFib _ 1 = 1
    memoFib f n = f (n - 1) + f (n - 2)
module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)