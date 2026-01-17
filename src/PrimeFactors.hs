module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n = factors n (2 : [3,5..])
  where
    factors 1 _ = []
    factors m (p:ps)
        | p * p > m = [m]
        | m `mod` p == 0 = p : factors (m `div` p) (p:ps)
        | otherwise = factors m ps