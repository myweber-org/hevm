module FibonacciSequence where

fibonacci :: Int -> [Int]
fibonacci n
    | n <= 0    = []
    | otherwise = takeWhile (<= n) fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)module FibonacciSequence where

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

import Data.Function (fix)

fibonacci :: Int -> Integer
fibonacci n = fibList !! n
  where
    fibList = 0 : 1 : zipWith (+) fibList (tail fibList)

-- Alternative implementation using fix for demonstration
fibonacciFix :: Int -> Integer
fibonacciFix = fix (\rec n ->
    if n <= 1
        then fromIntegral n
        else rec (n - 1) + rec (n - 2))

main :: IO ()
main = do
    putStrLn "Fibonacci sequence:"
    mapM_ (print . fibonacci) [0..10]module FibonacciSequence where

import Data.Function (fix)

fibonacci :: Int -> Integer
fibonacci = (map fib [0..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibonacci (n - 1) + fibonacci (n - 2)

-- Alternative implementation using fix for memoization
fibonacciMemoized :: Int -> Integer
fibonacciMemoized = fix memoFib
  where
    memoFib _ 0 = 0
    memoFib _ 1 = 1
    memoFib f n = f (n - 1) + f (n - 2)module FibonacciSequence where

import Data.Function (fix)

fibonacci :: Int -> Integer
fibonacci = (map fib [0..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibonacci (n - 1) + fibonacci (n - 2)

-- More efficient version using memoization with fixpoint combinator
fibonacciMemoized :: Int -> Integer
fibonacciMemoized = fix memoFib
  where
    memoFib _ 0 = 0
    memoFib _ 1 = 1
    memoFib f n = f (n - 1) + f (n - 2)