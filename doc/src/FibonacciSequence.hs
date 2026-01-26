module FibonacciSequence where

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