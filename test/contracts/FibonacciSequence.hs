module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)module FibonacciSequence where

import Data.Function (fix)

fibMemo :: Int -> Integer
fibMemo = (map fib [0..] !!)
    where
        fib 0 = 0
        fib 1 = 1
        fib n = fibMemo (n-1) + fibMemo (n-2)

fibonacci :: Int -> Integer
fibonacci = fix fibMemomodule FibonacciSequence where

fib :: Int -> Integer
fib n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n
    | n <= 0    = []
    | n == 1    = [0]
    | n == 2    = [0, 1]
    | otherwise = let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
                  in take n fibs

fibonacciWithIndex :: Int -> [(Int, Integer)]
fibonacciWithIndex n = zip [0..] (fibonacci n)

printFibonacciSequence :: Int -> IO ()
printFibonacciSequence n = do
    putStrLn $ "Fibonacci sequence up to " ++ show n ++ " terms:"
    mapM_ (\(i, val) -> putStrLn $ "F(" ++ show i ++ ") = " ++ show val) 
          (fibonacciWithIndex n)