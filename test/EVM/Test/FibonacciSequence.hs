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
fibonacciMemoized = fix memo
  where
    memo _ 0 = 0
    memo _ 1 = 1
    memo f n = f (n - 1) + f (n - 2)