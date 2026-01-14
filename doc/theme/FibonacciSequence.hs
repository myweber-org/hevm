module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n
    | n <= 0    = []
    | n == 1    = [0]
    | n == 2    = [0, 1]
    | otherwise = let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
                  in take n fibs

fibonacciRecursive :: Int -> [Integer]
fibonacciRecursive n = take n fibs
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
    putStrLn "First 10 Fibonacci numbers:"
    print (fibonacci 10)
    putStrLn "\nFirst 15 Fibonacci numbers (recursive style):"
    print (fibonacciRecursive 15)module FibonacciSequence where

import Data.Function.Memoize (memoize)

fib :: Integer -> Integer
fib = memoize fib'
  where
    fib' 0 = 0
    fib' 1 = 1
    fib' n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
    putStrLn "Fibonacci numbers:"
    mapM_ (print . fib) [0..10]