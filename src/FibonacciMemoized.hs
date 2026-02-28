module FibonacciMemoized where

import Data.Function.Memoize

fibonacci :: Int -> Integer
fibonacci = memoize fib
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibonacci (n - 1) + fibonacci (n - 2)

main :: IO ()
main = do
    putStrLn "First 20 Fibonacci numbers:"
    mapM_ (print . fibonacci) [0..19]module FibonacciMemoized where

import qualified Data.Map as Map

fib :: Int -> Integer
fib n = fst $ fibMemo Map.empty n
  where
    fibMemo :: Map.Map Int (Integer, Integer) -> Int -> (Integer, Map.Map Int (Integer, Integer))
    fibMemo cache 0 = (0, Map.insert 0 (0, 1) cache)
    fibMemo cache 1 = (1, Map.insert 1 (1, 1) cache)
    fibMemo cache n =
      case Map.lookup n cache of
        Just (result, _) -> (result, cache)
        Nothing ->
          let (a, cache1) = fibMemo cache (n - 1)
              (b, cache2) = fibMemo cache1 (n - 2)
              result = a + b
          in (result, Map.insert n (result, a) cache2)

fibFast :: Int -> Integer
fibFast n = fst $ fibMemoFast Map.empty n
  where
    fibMemoFast :: Map.Map Int Integer -> Int -> (Integer, Map.Map Int Integer)
    fibMemoFast cache n
      | n <= 1 = (fromIntegral n, cache)
      | otherwise =
          case Map.lookup n cache of
            Just result -> (result, cache)
            Nothing ->
              let (a, cache1) = fibMemoFast cache (n - 1)
                  (b, cache2) = fibMemoFast cache1 (n - 2)
                  result = a + b
              in (result, Map.insert n result cache2)