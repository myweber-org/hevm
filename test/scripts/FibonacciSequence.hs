module FibonacciSequence where

fib :: Int -> Integer
fib n = fibMemo !! n
  where
    fibMemo = map fib' [0..]
    fib' 0 = 0
    fib' 1 = 1
    fib' i = fibMemo !! (i - 1) + fibMemo !! (i - 2)module Fibonacci where

import Data.Map (Map)
import qualified Data.Map as Map

fibonacci :: Int -> Integer
fibonacci n = fst (fibMemo n)

fibMemo :: Int -> (Integer, Map Int Integer)
fibMemo n = go n Map.empty
  where
    go 0 m = (0, Map.insert 0 0 m)
    go 1 m = (1, Map.insert 1 1 m)
    go k m =
      case Map.lookup k m of
        Just val -> (val, m)
        Nothing ->
          let (a, m1) = go (k - 1) m
              (b, m2) = go (k - 2) m1
              sumVal = a + b
          in (sumVal, Map.insert k sumVal m2)

main :: IO ()
main = do
  putStrLn "First 20 Fibonacci numbers:"
  mapM_ (putStrLn . show . fibonacci) [0..19]