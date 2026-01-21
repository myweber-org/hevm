module FibonacciSequence where

import Data.Function.Memoize

fib :: Integer -> Integer
fib = memoize fib'
  where
    fib' 0 = 0
    fib' 1 = 1
    fib' n = fib (n - 1) + fib (n - 2)