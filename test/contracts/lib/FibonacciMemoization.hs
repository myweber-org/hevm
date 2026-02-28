module FibonacciMemoization where

import qualified Data.Map as Map

fib :: Int -> Integer
fib n = Map.findWithDefault 0 n memo
  where
    memo = Map.fromList [(i, fib' i) | i <- [0..n]]
    fib' 0 = 0
    fib' 1 = 1
    fib' x = memo Map.! (x-1) + memo Map.! (x-2)