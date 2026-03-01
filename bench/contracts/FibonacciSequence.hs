
module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)module FibonacciSequence where

import Data.Function (fix)

fibMemo :: Int -> Integer
fibMemo n = fibList !! n
  where
    fibList = map fib [0..]
    fib 0 = 0
    fib 1 = 1
    fib i = fibList !! (i - 1) + fibList !! (i - 2)

fibMemoFix :: Int -> Integer
fibMemoFix = fix (\rec n -> if n < 2 then fromIntegral n else rec (n - 1) + rec (n - 2))module FibonacciSequence where

import Data.Function (fix)

fib :: Int -> Integer
fib n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibMemoized :: Int -> Integer
fibMemoized = fix (\rec n -> if n < 2 then fromIntegral n else rec (n - 1) + rec (n - 2))module FibonacciSequence where

import Data.Map (Map)
import qualified Data.Map as Map

fib :: Int -> Integer
fib n = fst (fibMemo n Map.empty)

fibMemo :: Int -> Map Int Integer -> (Integer, Map Int Integer)
fibMemo n cache
    | n <= 1    = (fromIntegral n, cache)
    | otherwise = case Map.lookup n cache of
        Just val -> (val, cache)
        Nothing  -> let (a, cache1) = fibMemo (n-1) cache
                        (b, cache2) = fibMemo (n-2) cache1
                        result = a + b
                    in (result, Map.insert n result cache2)

printFibonacciSequence :: Int -> IO ()
printFibonacciSequence n = do
    putStrLn $ "First " ++ show n ++ " Fibonacci numbers:"
    mapM_ (putStrLn . show . fib) [0..n-1]