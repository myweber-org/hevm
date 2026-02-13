module Fibonacci where

fibonacci :: Integer -> Integer
fibonacci n
    | n < 0 = error "Negative input not allowed"
    | otherwise = fst (fibPair n)
  where
    fibPair :: Integer -> (Integer, Integer)
    fibPair 0 = (0, 1)
    fibPair k
        | even k = (a, b)
        | otherwise = (b, a + b)
      where
        (a, b) = fibPair (k `div` 2)
        c = a * (b * 2 - a)
        d = a * a + b * b
        (a, b) = if even k then (c, d) else (d, c + d)