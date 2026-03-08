module FibonacciSequence where

import Data.Function.Memoize (memoize)

fibonacci :: Int -> Integer
fibonacci = memoize fib
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibonacci (n - 1) + fibonacci (n - 2)