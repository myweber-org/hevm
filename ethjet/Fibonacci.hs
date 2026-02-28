
module Fibonacci where

import Data.Function (fix)

fibonacci :: Int -> Integer
fibonacci = (map fib [0..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibonacci (n - 1) + fibonacci (n - 2)

fibonacciMemoized :: Int -> Integer
fibonacciMemoized = fix memoize
  where
    memoize _ 0 = 0
    memoize _ 1 = 1
    memoize f n = f (n - 1) + f (n - 2)