
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
fibMemoized = fix (\rec n -> if n < 2 then fromIntegral n else rec (n - 1) + rec (n - 2))