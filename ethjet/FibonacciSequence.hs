module Fibonacci where

fibonacci :: Integer -> Integer
fibonacci n
    | n < 0 = error "Negative input not allowed"
    | otherwise = fst (fibPair n)
  where
    fibPair :: Integer -> (Integer, Integer)
    fibPair 0 = (0, 1)
    fibPair k
        | even k = (a, b)
        | otherwise = (b, a + b)
      where
        (a, b) = fibPair (k `div` 2)
        c = a * (b * 2 - a)
        d = a * a + b * b
        (a, b) = if even k then (c, d) else (d, c + d)module FibonacciSequence where

fibonacci :: Int -> [Int]
fibonacci n = takeWhile (<= n) fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

printFibonacci :: Int -> IO ()
printFibonacci n = do
  putStrLn $ "Fibonacci sequence up to " ++ show n ++ " terms:"
  mapM_ (putStrLn . show) (fibonacci n)module FibonacciSequence where

import Data.Function (fix)

fibonacci :: Int -> Integer
fibonacci n = fibList !! n
  where
    fibList = 0 : 1 : zipWith (+) fibList (tail fibList)

-- Alternative implementation using fix for memoization
fibonacci' :: Int -> Integer
fibonacci' = fix (\rec n -> if n < 2 then fromIntegral n else rec (n-1) + rec (n-2))

main :: IO ()
main = do
    putStrLn "First 20 Fibonacci numbers:"
    mapM_ (print . fibonacci) [0..19]module FibonacciSequence where

import Data.Map (Map)
import qualified Data.Map as Map

fib :: Int -> Integer
fib n = fibMemo n Map.empty
  where
    fibMemo :: Int -> Map Int Integer -> (Integer, Map Int Integer)
    fibMemo 0 memo = (0, memo)
    fibMemo 1 memo = (1, memo)
    fibMemo x memo =
      case Map.lookup x memo of
        Just result -> (result, memo)
        Nothing ->
          let (a, memo1) = fibMemo (x - 1) memo
              (b, memo2) = fibMemo (x - 2) memo1
              result = a + b
          in (result, Map.insert x result memo2)

main :: IO ()
main = do
  putStrLn "First 20 Fibonacci numbers:"
  mapM_ (print . fib) [0..19]module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)