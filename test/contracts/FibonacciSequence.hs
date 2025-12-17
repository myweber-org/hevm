module FibonacciSequence where

fib :: Int -> Integer
fib n = fibMemo !! n
  where
    fibMemo = map fib' [0..]
    fib' 0 = 0
    fib' 1 = 1
    fib' i = fibMemo !! (i - 1) + fibMemo !! (i - 2)module FibonacciSequence where

import Data.Map (Map)
import qualified Data.Map as Map

fibonacci :: Int -> Integer
fibonacci n = fst (fibMemo n Map.empty)

fibMemo :: Int -> Map Int Integer -> (Integer, Map Int Integer)
fibMemo n memo
    | n <= 1    = (fromIntegral n, memo)
    | otherwise = case Map.lookup n memo of
        Just val -> (val, memo)
        Nothing  -> let (a, memo1) = fibMemo (n-1) memo
                        (b, memo2) = fibMemo (n-2) memo1
                        val = a + b
                    in (val, Map.insert n val memo2)

main :: IO ()
main = do
    putStrLn "First 20 Fibonacci numbers:"
    mapM_ (print . fibonacci) [0..19]