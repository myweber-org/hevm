module PrimeFactors where

primeFactors :: Integer -> [Integer]
primeFactors n
    | n <= 1    = []
    | otherwise = factor n 2
    where
        factor m p
            | m == 1         = []
            | m `mod` p == 0 = p : factor (m `div` p) p
            | otherwise      = factor m (p + 1)