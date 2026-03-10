module FibonacciSequence where

import Data.Map (Map)
import qualified Data.Map as Map

fibonacci :: Int -> Integer
fibonacci n = fst (fibMemo n Map.empty)

fibMemo :: Int -> Map Int Integer -> (Integer, Map Int Integer)
fibMemo n memo
    | n <= 1 = (fromIntegral n, memo)
    | otherwise = case Map.lookup n memo of
        Just val -> (val, memo)
        Nothing -> let
            (a, memo1) = fibMemo (n-1) memo
            (b, memo2) = fibMemo (n-2) memo1
            result = a + b
            in (result, Map.insert n result memo2)

fibonacciList :: Int -> [Integer]
fibonacciList n = map fibonacci [0..n-1]

main :: IO ()
main = do
    putStrLn "First 20 Fibonacci numbers:"
    print $ fibonacciList 20
    putStrLn "\nIndividual Fibonacci numbers:"
    print $ fibonacci 10
    print $ fibonacci 20
    print $ fibonacci 30module FibonacciSequence where

fibonacci :: Integer -> [Integer]
fibonacci n = takeWhile (<= n) fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)