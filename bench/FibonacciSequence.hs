module Fibonacci where

fib :: Int -> Integer
fib n = fibs !! n
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
    putStrLn "Fibonacci sequence up to 20:"
    mapM_ (print . fib) [0..20]module FibonacciSequence where

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)module FibonacciSequence where

import Data.Function.Memoize (memoize)

fib :: Int -> Integer
fib = memoize fib'
  where
    fib' 0 = 0
    fib' 1 = 1
    fib' n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
    putStrLn "Fibonacci numbers from 0 to 20:"
    mapM_ (print . fib) [0..20]module FibonacciSequence where

import Data.Function.Memoize

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

fibMemo :: Int -> Integer
fibMemo = (map fib [0..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibMemo (n - 1) + fibMemo (n - 2)

fastFib :: Int -> Integer
fastFib = fix (\f n -> if n < 2 then fromIntegral n else f (n - 1) + f (n - 2))module FibonacciSequence where

import Data.Function (fix)

fibMemo :: Int -> Integer
fibMemo = fix memo
  where
    memo _ 0 = 0
    memo _ 1 = 1
    memo f n = f (n - 1) + f (n - 2)module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
    putStrLn "Fibonacci sequence up to 10 terms:"
    print $ fibonacci 10