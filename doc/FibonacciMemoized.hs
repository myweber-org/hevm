module FibonacciMemoized where

import qualified Data.Map as Map

fibMemo :: Int -> Integer
fibMemo n = Map.findWithDefault (fib' n) n memoMap
  where
    memoMap = Map.fromList [(0, 0), (1, 1)]
    fib' 0 = 0
    fib' 1 = 1
    fib' x = fibMemo (x - 1) + fibMemo (x - 2)

main :: IO ()
main = do
    putStrLn "Fibonacci numbers from 0 to 20:"
    mapM_ (putStrLn . show . fibMemo) [0..20]