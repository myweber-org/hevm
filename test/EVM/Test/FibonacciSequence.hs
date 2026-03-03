
module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
  putStrLn "Fibonacci sequence up to 10 terms:"
  print $ fibonacci 10module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

printFibonacci :: Int -> IO ()
printFibonacci n = do
  putStrLn $ "First " ++ show n ++ " Fibonacci numbers:"
  mapM_ (putStrLn . show) (fibonacci n)module FibonacciSequence where

import Data.Function (fix)

fibMemo :: Int -> Integer
fibMemo = (map fib [0..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibMemo (n - 1) + fibMemo (n - 2)

fibonacci :: Int -> Integer
fibonacci = fix (\f n -> if n < 2 then fromIntegral n else f (n - 1) + f (n - 2))