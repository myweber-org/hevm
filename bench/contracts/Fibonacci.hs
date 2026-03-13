module Fibonacci where

import Data.Map (Map)
import qualified Data.Map as Map

fibMemo :: Int -> Integer
fibMemo n = fst (fibMemo' n Map.empty)

fibMemo' :: Int -> Map Int Integer -> (Integer, Map Int Integer)
fibMemo' n memo
    | n <= 1 = (fromIntegral n, memo)
    | otherwise =
        case Map.lookup n memo of
            Just val -> (val, memo)
            Nothing ->
                let (a, memo1) = fibMemo' (n - 1) memo
                    (b, memo2) = fibMemo' (n - 2) memo1
                    val = a + b
                in (val, Map.insert n val memo2)