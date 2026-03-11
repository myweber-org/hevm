module PrimeSieve where

primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

isPrime :: Integer -> Bool
isPrime n
    | n < 2     = False
    | otherwise = all (\p -> n `mod` p /= 0) (takeWhile (\p -> p * p <= n) primes)

primeFactors :: Integer -> [Integer]
primeFactors n = factors n primes
  where
    factors 1 _ = []
    factors m (p:ps)
        | p * p > m = [m]
        | m `mod` p == 0 = p : factors (m `div` p) (p:ps)
        | otherwise = factors m ps