
module Fibonacci where

fib :: Int -> Integer
fib n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
  putStrLn "First 20 Fibonacci numbers:"
  mapM_ (print . fib) [0..19]