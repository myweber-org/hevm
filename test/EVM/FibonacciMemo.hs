module FibonacciMemo where

import qualified Data.Map as Map

fibMemo :: Int -> Integer
fibMemo n = fst (fibMemo' n Map.empty)

fibMemo' :: Int -> Map.Map Int Integer -> (Integer, Map.Map Int Integer)
fibMemo' n cache
  | n <= 1    = (fromIntegral n, cache)
  | otherwise = case Map.lookup n cache of
      Just val -> (val, cache)
      Nothing  -> let (a, cache1) = fibMemo' (n-1) cache
                      (b, cache2) = fibMemo' (n-2) cache1
                      result = a + b
                  in (result, Map.insert n result cache2)