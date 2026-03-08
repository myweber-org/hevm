module FibonacciMemoization where

import qualified Data.Map as Map

fib :: Int -> Integer
fib n = Map.findWithDefault 0 n memo
  where
    memo = Map.fromList [(i, fib' i) | i <- [0..n]]
    fib' 0 = 0
    fib' 1 = 1
    fib' x = memo Map.! (x-1) + memo Map.! (x-2)module FibonacciMemoization where

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibonacci :: Int -> Integer
fibonacci n = fibs !! n

main :: IO ()
main = do
    putStrLn "First 20 Fibonacci numbers:"
    print $ take 20 fibs
    putStrLn "\nFibonacci number at position 50:"
    print $ fibonacci 50