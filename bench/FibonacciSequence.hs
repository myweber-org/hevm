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
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)