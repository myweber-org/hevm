module FibonacciSequence where

import Data.Function (fix)

fibonacci :: Int -> Integer
fibonacci n = fibList !! n
  where
    fibList = 0 : 1 : zipWith (+) fibList (tail fibList)

-- Alternative implementation using fix for demonstration
fibonacciFix :: Int -> Integer
fibonacciFix = fix (\rec n -> if n < 2 then fromIntegral n else rec (n-1) + rec (n-2))module FibonacciSequence where

fib :: Int -> Integer
fib n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)module FibonacciSequence where

import Data.Function.Memoize

fib :: Integer -> Integer
fib = memoize fib'
  where
    fib' 0 = 0
    fib' 1 = 1
    fib' n = fib (n - 1) + fib (n - 2)module FibonacciSequence where

import Data.Function (fix)

fibMemo :: Int -> Integer
fibMemo = fix memoize
  where
    memoize _ 0 = 0
    memoize _ 1 = 1
    memoize f n = f (n - 1) + f (n - 2)

main :: IO ()
main = do
    putStrLn "Fibonacci numbers from 0 to 20:"
    mapM_ (print . fibMemo) [0..20]module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

printFibonacci :: Int -> IO ()
printFibonacci n = do
    putStrLn $ "Fibonacci sequence up to " ++ show n ++ " terms:"
    mapM_ (putStrLn . show) (fibonacci n)

main :: IO ()
main = do
    putStr "Enter number of terms: "
    input <- getLine
    let n = read input :: Int
    if n > 0
        then printFibonacci n
        else putStrLn "Please enter a positive integer."module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

printFibonacci :: Int -> IO ()
printFibonacci n = do
    putStrLn $ "First " ++ show n ++ " Fibonacci numbers:"
    mapM_ (putStr . (++ " ") . show) (fibonacci n)
    putStrLn ""