module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n
    | n <= 0    = []
    | n == 1    = [0]
    | n == 2    = [0, 1]
    | otherwise = let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
                  in take n fibs

main :: IO ()
main = do
    putStrLn "Fibonacci sequence up to 10 terms:"
    print $ fibonacci 10module FibonacciSequence where

import Data.Function (fix)

fibonacci :: Int -> Integer
fibonacci n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibonacciMemoized :: Int -> Integer
fibonacciMemoized = fix (\rec n -> if n < 2 then fromIntegral n else rec (n - 1) + rec (n - 2))

main :: IO ()
main = do
    putStrLn "Fibonacci numbers from 0 to 10:"
    mapM_ (print . fibonacci) [0..10]
    putStrLn "\nUsing memoized version for index 15:"
    print $ fibonacciMemoized 15module FibonacciSequence where

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

fibMemo :: Int -> Integer
fibMemo = (map fib [0..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibMemo (n - 1) + fibMemo (n - 2)

fastFib :: Int -> Integer
fastFib = fix (\f n -> if n < 2 then fromIntegral n else f (n - 1) + f (n - 2))module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)module FibonacciSequence where

fibonacci :: Int -> Integer
fibonacci = (map fib [0..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibonacci (n - 1) + fibonacci (n - 2)

main :: IO ()
main = do
    putStrLn "First 20 Fibonacci numbers:"
    mapM_ (print . fibonacci) [0..19]