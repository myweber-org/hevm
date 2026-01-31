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
    mapM_ (print . fib) [0..10]module FibonacciSequence where

import Data.Map (Map)
import qualified Data.Map as Map

fibonacci :: Int -> Integer
fibonacci = (map fib [0..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibonacci (n - 1) + fibonacci (n - 2)

memoizedFibonacci :: Int -> Integer
memoizedFibonacci = (map fib [0..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = memoizedFibonacci (n - 1) + memoizedFibonacci (n - 2)

fibonacciMemo :: Int -> Integer
fibonacciMemo = memoize fib
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibonacciMemo (n - 1) + fibonacciMemo (n - 2)

memoize :: (Int -> a) -> Int -> a
memoize f = let cache = Map.fromList [(x, f x) | x <- [0..]] 
            in \n -> cache Map.! n

main :: IO ()
main = do
    putStrLn "Fibonacci sequence:"
    mapM_ (print . fibonacci) [0..10]
    putStrLn "\nMemoized version:"
    mapM_ (print . memoizedFibonacci) [0..10]