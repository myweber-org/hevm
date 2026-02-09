module FibonacciMemoized where

import Data.Function.Memoize

fibonacci :: Int -> Integer
fibonacci = memoize fib
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibonacci (n - 1) + fibonacci (n - 2)

main :: IO ()
main = do
    putStrLn "First 20 Fibonacci numbers:"
    mapM_ (print . fibonacci) [0..19]