module FibonacciSequence where

fibonacci :: Int -> Integer
fibonacci n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
    putStrLn "Fibonacci sequence:"
    mapM_ (print . fibonacci) [0..20]