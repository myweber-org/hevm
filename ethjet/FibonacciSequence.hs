module FibonacciSequence where

import Data.Function (fix)

fibonacci :: Int -> Integer
fibonacci n = fibList !! n
  where
    fibList = 0 : 1 : zipWith (+) fibList (tail fibList)

-- Alternative implementation using fix for memoization
fibonacci' :: Int -> Integer
fibonacci' = fix (\rec n -> if n < 2 then fromIntegral n else rec (n-1) + rec (n-2))

-- Fast doubling algorithm for O(log n) computation
fibFastDoubling :: Int -> Integer
fibFastDoubling n
    | n < 0     = error "Negative input not supported"
    | otherwise = fst (fibPair n)
  where
    fibPair 0 = (0, 1)
    fibPair k =
        let (a, b) = fibPair (k `div` 2)
            c = a * (b * 2 - a)
            d = a * a + b * b
        in if even k
            then (c, d)
            else (d, c + d)module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

printFibonacci :: Int -> IO ()
printFibonacci n = do
  putStrLn $ "Fibonacci sequence up to " ++ show n ++ " terms:"
  mapM_ (putStr . (++ " ") . show) (fibonacci n)
  putStrLn ""module FibonacciSequence where

import Data.Function (fix)

fibonacci :: Int -> Integer
fibonacci = (map fib [0..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibonacci (n - 1) + fibonacci (n - 2)

-- More efficient version using memoization with fixpoint combinator
fibonacciMemoized :: Int -> Integer
fibonacciMemoized = fix memoFib
  where
    memoFib _ 0 = 0
    memoFib _ 1 = 1
    memoFib f n = f (n - 1) + f (n - 2)module FibonacciSequence where

fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)