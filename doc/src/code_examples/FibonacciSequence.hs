module FibonacciSequence where

fibonacci :: Integer -> Integer
fibonacci n
    | n <= 0    = 0
    | n == 1    = 1
    | otherwise = fibonacci (n - 1) + fibonacci (n - 2)module FibonacciSequence where

import Data.Function (fix)

fibonacci :: Int -> Integer
fibonacci = (map fib [0..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibonacci (n - 1) + fibonacci (n - 2)

-- More efficient version using memoization with fixpoint combinator
fibonacciMemoized :: Int -> Integer
fibonacciMemoized = fix memoFib
  where
    memoFib _ 0 = 0
    memoFib _ 1 = 1
    memoFib f n = f (n - 1) + f (n - 2)