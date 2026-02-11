module FibonacciSequence where

fibonacci :: Int -> Integer
fibonacci = (map fib [0..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibonacci (n - 1) + fibonacci (n - 2)

main :: IO ()
main = do
    putStrLn "Fibonacci sequence up to 10:"
    mapM_ (print . fibonacci) [0..10]