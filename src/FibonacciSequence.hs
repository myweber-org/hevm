module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
    putStrLn "Enter the number of Fibonacci terms:"
    input <- getLine
    let n = read input :: Int
    print $ fibonacci n