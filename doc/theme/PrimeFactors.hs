
module PrimeFactors where

primeFactors :: Integer -> [Integer]
primeFactors n
  | n <= 1    = []
  | otherwise = factorize n 2
  where
    factorize 1 _ = []
    factorize m d
      | m `mod` d == 0 = d : factorize (m `div` d) d
      | otherwise      = factorize m (d + 1)