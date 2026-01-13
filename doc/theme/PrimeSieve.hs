module PrimeSieve where

primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- Function to get the first n prime numbers
firstNPrimes :: Int -> [Int]
firstNPrimes n = take n primes

-- Example usage in GHCi:
-- firstNPrimes 10 -> [2,3,5,7,11,13,17,19,23,29]