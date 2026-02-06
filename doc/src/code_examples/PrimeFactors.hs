module PrimeFactors where

primeFactors :: Integer -> [Integer]
primeFactors n = factors n 2
  where
    factors 1 _ = []
    factors m p
      | m < p * p = [m]
      | m `mod` p == 0 = p : factors (m `div` p) p
      | otherwise = factors m (p + 1)module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n
  | n <= 1    = []
  | otherwise = factorize n 2
  where
    factorize 1 _ = []
    factorize m d
      | m `mod` d == 0 = d : factorize (m `div` d) d
      | otherwise      = factorize m (nextDivisor d)
    
    nextDivisor 2 = 3
    nextDivisor d = d + 2