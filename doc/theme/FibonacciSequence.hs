module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n
    | n <= 0    = []
    | n == 1    = [0]
    | n == 2    = [0, 1]
    | otherwise = let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
                  in take n fibs

fibonacciRecursive :: Int -> [Integer]
fibonacciRecursive n = take n fibs
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
    putStrLn "First 10 Fibonacci numbers:"
    print (fibonacci 10)
    putStrLn "\nFirst 15 Fibonacci numbers (recursive style):"
    print (fibonacciRecursive 15)