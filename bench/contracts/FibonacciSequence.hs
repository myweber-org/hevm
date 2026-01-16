module FibonacciSequence where

import Data.Function (fix)

fibonacci :: Int -> Integer
fibonacci = (map fib [0..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibonacci (n - 1) + fibonacci (n - 2)

memoizedFibonacci :: Int -> Integer
memoizedFibonacci = fix (memoize . fib)
  where
    fib f 0 = 0
    fib f 1 = 1
    fib f n = f (n - 1) + f (n - 2)
    
    memoize f = (map f [0..] !!)

main :: IO ()
main = do
    putStrLn "First 20 Fibonacci numbers:"
    mapM_ (print . memoizedFibonacci) [0..19]