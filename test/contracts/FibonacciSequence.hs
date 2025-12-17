module FibonacciSequence where

fib :: Int -> Integer
fib n = fibMemo !! n
  where
    fibMemo = map fib' [0..]
    fib' 0 = 0
    fib' 1 = 1
    fib' i = fibMemo !! (i - 1) + fibMemo !! (i - 2)