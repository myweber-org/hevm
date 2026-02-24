module FibonacciMemo where

import qualified Data.Map as Map

fibMemo :: Int -> Integer
fibMemo n = Map.findWithDefault (fib' n) n memoMap
  where
    fib' 0 = 0
    fib' 1 = 1
    fib' x = fibMemo (x - 1) + fibMemo (x - 2)
    
    memoMap = Map.fromList [(0,0), (1,1)]

main :: IO ()
main = do
    putStrLn "Fibonacci numbers with memoization:"
    mapM_ (print . fibMemo) [0..20]