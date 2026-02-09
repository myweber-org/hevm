module FibonacciMemoized where

import Data.Map (Map)
import qualified Data.Map as Map

fibMemo :: Int -> Integer
fibMemo = let fibCache = Map.fromList [(0, 0), (1, 1)]
              fib' n = case Map.lookup n fibCache of
                Just x -> x
                Nothing -> let x = fib' (n-1) + fib' (n-2)
                           in x `seq` Map.insert n x fibCache
          in \n -> fib' n

main :: IO ()
main = do
    putStrLn "Fibonacci numbers with memoization:"
    mapM_ (print . fibMemo) [0..20]