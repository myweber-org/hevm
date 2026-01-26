module Fibonacci where

fibonacci :: Int -> Integer
fibonacci n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)module Fibonacci where

fibonacci :: Int -> Integer
fibonacci = (map fib [0..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibonacci (n-1) + fibonacci (n-2)

main :: IO ()
main = do
    putStrLn "First 20 Fibonacci numbers:"
    mapM_ (print . fibonacci) [0..19]module FibonacciSequence where

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
    mapM_ (print . fibonacci) [0..19]