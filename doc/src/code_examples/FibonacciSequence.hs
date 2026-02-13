module FibonacciSequence where

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
    print $ fibonacci 10