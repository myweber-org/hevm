module FibonacciSequence where

fibonacci :: Integer -> [Integer]
fibonacci n = takeWhile (<= n) fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
    putStrLn "Enter a number:"
    input <- getLine
    let n = read input :: Integer
    putStrLn $ "Fibonacci numbers up to " ++ show n ++ ":"
    print $ fibonacci n