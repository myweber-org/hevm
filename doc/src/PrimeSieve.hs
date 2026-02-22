module PrimeSieve where

sieve :: Int -> [Int]
sieve limit
    | limit < 2 = []
    | otherwise = go [2..limit] []
  where
    go [] primes = reverse primes
    go (x:xs) primes = go (filter (\n -> n `mod` x /= 0) xs) (x:primes)

primesUpTo :: Int -> [Int]
primesUpTo = sievemodule PrimeSieve where

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