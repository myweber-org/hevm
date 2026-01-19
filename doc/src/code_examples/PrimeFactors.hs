module PrimeFactors where

primeFactors :: Integer -> [Integer]
primeFactors n = factors n 2
  where
    factors 1 _ = []
    factors m p
      | m < p * p = [m]
      | m `mod` p == 0 = p : factors (m `div` p) p
      | otherwise = factors m (p + 1)