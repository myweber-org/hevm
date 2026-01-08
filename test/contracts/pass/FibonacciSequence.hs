
module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n
    | n <= 0    = []
    | n == 1    = [0]
    | n == 2    = [0, 1]
    | otherwise = let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
                  in take n fibs

fibonacciMemoized :: Int -> [Integer]
fibonacciMemoized n = take n fibs
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
    putStrLn "Fibonacci sequence (first 10 terms):"
    print $ fibonacci 10
    putStrLn "\nMemoized version (first 15 terms):"
    print $ fibonacciMemoized 15