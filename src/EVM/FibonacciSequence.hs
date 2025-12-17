module FibonacciSequence where

fibonacci :: Int -> [Int]
fibonacci n
    | n <= 0    = []
    | n == 1    = [0]
    | n == 2    = [0, 1]
    | otherwise = let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
                  in take n fibs

fibonacciUpTo :: Int -> [Int]
fibonacciUpTo limit = takeWhile (<= limit) $ fibonacci (limit + 1)