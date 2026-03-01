module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n
    | n <= 0    = []
    | n == 1    = [0]
    | n == 2    = [0, 1]
    | otherwise = let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
                  in take n fibs

fibonacciSum :: Int -> Integer
fibonacciSum n = sum (fibonacci n)

main :: IO ()
main = do
    putStrLn "Enter number of Fibonacci terms:"
    input <- getLine
    let n = read input :: Int
    putStrLn $ "Fibonacci sequence: " ++ show (fibonacci n)
    putStrLn $ "Sum of first " ++ show n ++ " terms: " ++ show (fibonacciSum n)module FibonacciSequence where

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
    fib n = fibMemo (n - 1) + fibMemo (n - 2)

main :: IO ()
main = do
    putStrLn "Fibonacci numbers from 0 to 20:"
    mapM_ (print . fibMemo) [0..20]