module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n
  | n <= 1    = []
  | otherwise = factorize n 2
  where
    factorize 1 _ = []
    factorize m p
      | m < p * p  = [m]
      | m `mod` p == 0 = p : factorize (m `div` p) p
      | otherwise      = factorize m (nextPrime p)
    
    nextPrime 2 = 3
    nextPrime p = p + 2