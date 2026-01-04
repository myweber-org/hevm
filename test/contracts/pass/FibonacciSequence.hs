module FibonacciSequence where

import Data.Map (Map)
import qualified Data.Map as Map

fib :: Int -> Integer
fib n = fst $ fibMemo n Map.empty

fibMemo :: Int -> Map Int Integer -> (Integer, Map Int Integer)
fibMemo 0 memo = (0, Map.insert 0 0 memo)
fibMemo 1 memo = (1, Map.insert 1 1 memo)
fibMemo n memo =
    case Map.lookup n memo of
        Just val -> (val, memo)
        Nothing ->
            let (a, memo1) = fibMemo (n-1) memo
                (b, memo2) = fibMemo (n-2) memo1
                result = a + b
            in (result, Map.insert n result memo2)

printFibonacciSequence :: Int -> IO ()
printFibonacciSequence n = do
    putStrLn $ "Fibonacci sequence up to " ++ show n ++ ":"
    mapM_ (putStrLn . show . fib) [0..n]module FibonacciSequence where

fibonacci :: Int -> Integer
fibonacci n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)