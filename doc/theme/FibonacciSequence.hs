module FibonacciSequence where

import Data.Function.Memoize (memoize)

fib :: Integer -> Integer
fib = memoize fib'
  where
    fib' 0 = 0
    fib' 1 = 1
    fib' n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
    putStrLn "Fibonacci numbers from 0 to 10:"
    mapM_ (print . fib) [0..10]module FibonacciSequence where

fib :: Int -> Integer
fib n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
  putStrLn "Fibonacci sequence up to 10 terms:"
  print $ fibonacci 10module FibonacciSequence where

fibonacci :: Int -> [Int]
fibonacci n
    | n <= 0    = []
    | n == 1    = [0]
    | n == 2    = [0, 1]
    | otherwise = let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
                  in take n fibs

fibonacciUpTo :: Int -> [Int]
fibonacciUpTo limit = takeWhile (<= limit) fibs
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)module FibonacciSequence where

import Data.Map (Map)
import qualified Data.Map as Map

fibonacci :: Int -> Integer
fibonacci n = fst (fibMemo n Map.empty)

fibMemo :: Int -> Map Int Integer -> (Integer, Map Int Integer)
fibMemo n memo
  | n <= 1    = (fromIntegral n, memo)
  | otherwise = case Map.lookup n memo of
      Just val -> (val, memo)
      Nothing  -> let (a, memo1) = fibMemo (n-1) memo
                      (b, memo2) = fibMemo (n-2) memo1
                      result = a + b
                  in (result, Map.insert n result memo2)

fibonacciList :: Int -> [Integer]
fibonacciList n = map fibonacci [0..n-1]

main :: IO ()
main = do
  putStrLn "First 20 Fibonacci numbers:"
  print $ fibonacciList 20
  putStrLn "\nFibonacci number at position 30:"
  print $ fibonacci 30module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
  putStrLn "Fibonacci sequence up to 10 terms:"
  print $ fibonacci 10