module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n
    | n <= 0    = []
    | n == 1    = [0]
    | n == 2    = [0, 1]
    | otherwise = let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
                  in take n fibs

fibonacciSum :: Int -> Integer
fibonacciSum n = sum (fibonacci n)

main :: IO ()
main = do
    putStrLn "Enter number of Fibonacci terms:"
    input <- getLine
    let n = read input :: Int
    putStrLn $ "Fibonacci sequence: " ++ show (fibonacci n)
    putStrLn $ "Sum of first " ++ show n ++ " terms: " ++ show (fibonacciSum n)