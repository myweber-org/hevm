module Fibonacci where

import Data.Function (fix)

fibonacci :: Int -> Integer
fibonacci = (map fib [0..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibonacci (n - 1) + fibonacci (n - 2)

memoizedFibonacci :: Int -> Integer
memoizedFibonacci = fix (memoize . fib)
  where
    fib _ 0 = 0
    fib _ 1 = 1
    fib f n = f (n - 1) + f (n - 2)
    
    memoize :: (Int -> Integer) -> (Int -> Integer)
    memoize f = (map f [0..] !!)