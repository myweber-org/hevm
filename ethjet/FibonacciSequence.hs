module FibonacciSequence where

import Data.Function (fix)

fibonacci :: Int -> Integer
fibonacci n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibonacciMemoized :: Int -> Integer
fibonacciMemoized = fix (\rec n -> if n < 2 then fromIntegral n else rec (n - 1) + rec (n - 2))module FibonacciSequence where

fibonacci :: Integer -> Integer
fibonacci n
    | n < 0 = error "Negative input not allowed"
    | otherwise = fibHelper n 0 1
  where
    fibHelper 0 a _ = a
    fibHelper n a b = fibHelper (n - 1) b (a + b)module Fibonacci where

import Data.Function (fix)

fibonacci :: Int -> Integer
fibonacci = (map fib [0..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibonacci (n - 1) + fibonacci (n - 2)

-- Alternative implementation using fix for memoization
fibonacciMemo :: Int -> Integer
fibonacciMemo = fix memo
  where
    memo _ 0 = 0
    memo _ 1 = 1
    memo f n = f (n - 1) + f (n - 2)

main :: IO ()
main = do
    putStrLn "Fibonacci numbers from 0 to 20:"
    mapM_ (print . fibonacci) [0..20]