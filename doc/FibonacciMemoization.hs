module FibonacciMemoization where

import qualified Data.Map as Map

fibMemo :: Int -> Integer
fibMemo = (map fib [0..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibMemo (n-1) + fibMemo (n-2)

fibMemoMap :: Int -> Integer
fibMemoMap n = snd $ memoize n Map.empty
  where
    memoize :: Int -> Map.Map Int Integer -> (Map.Map Int Integer, Integer)
    memoize 0 m = (Map.insert 0 0 m, 0)
    memoize 1 m = (Map.insert 1 1 m, 1)
    memoize x m =
      case Map.lookup x m of
        Just result -> (m, result)
        Nothing ->
          let (m1, a) = memoize (x-1) m
              (m2, b) = memoize (x-2) m1
              result = a + b
          in (Map.insert x result m2, result)

main :: IO ()
main = do
  putStrLn "Testing memoized Fibonacci implementations:"
  print $ map fibMemo [0..20]
  print $ map fibMemoMap [0..20]