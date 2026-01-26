module FibonacciMemoized where

import qualified Data.Map as Map

fibMemo :: Int -> Integer
fibMemo = (map fib [0..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibMemo (n-1) + fibMemo (n-2)

fibMemoMap :: Int -> Integer
fibMemoMap n = Map.findWithDefault (calc n) n memoMap
  where
    memoMap = Map.fromList [(i, fibMemoMap i) | i <- [0..n]]
    calc 0 = 0
    calc 1 = 1
    calc x = fibMemoMap (x-1) + fibMemoMap (x-2)

main :: IO ()
main = do
    putStrLn "Fibonacci numbers using memoization:"
    mapM_ (print . fibMemo) [0..20]
    putStrLn "\nAlternative implementation using Data.Map:"
    mapM_ (print . fibMemoMap) [0..20]