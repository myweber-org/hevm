module FibonacciMemoization where

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibonacci :: Int -> Integer
fibonacci n = fibs !! n

main :: IO ()
main = do
    putStrLn "First 20 Fibonacci numbers:"
    mapM_ (print . fibonacci) [0..19]