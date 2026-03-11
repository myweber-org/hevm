module FibonacciMemoization where

import Data.Map (Map)
import qualified Data.Map as Map

fib :: Int -> Integer
fib = (map fib' [0..] !!)
  where
    fib' 0 = 0
    fib' 1 = 1
    fib' n = fib (n - 1) + fib (n - 2)

fibMemo :: Int -> Integer
fibMemo = go Map.empty
  where
    go :: Map Int Integer -> Int -> Integer
    go cache n
      | n <= 1 = fromIntegral n
      | otherwise = case Map.lookup n cache of
          Just result -> result
          Nothing ->
            let result = go cache (n - 1) + go cache (n - 2)
            in result `seq` go (Map.insert n result cache) n

main :: IO ()
main = do
  putStrLn "Fibonacci numbers with memoization:"
  mapM_ (print . fibMemo) [0..10]