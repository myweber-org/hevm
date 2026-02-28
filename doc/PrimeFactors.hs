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
module PrimeFactors where

primeFactors :: Integer -> [Integer]
primeFactors n = factors n 2
  where
    factors 1 _ = []
    factors m p
      | m < p * p = [m]
      | m `mod` p == 0 = p : factors (m `div` p) p
      | otherwise = factors m (p + 1)
module PrimeFactors where

primeFactors :: Integer -> [Integer]
primeFactors n
  | n <= 1 = []
  | otherwise = factorize n 2
  where
    factorize 1 _ = []
    factorize m p
      | m < p * p = [m]
      | m `mod` p == 0 = p : factorize (m `div` p) p
      | otherwise = factorize m (p + 1)