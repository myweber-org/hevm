module PrimeSieve where

primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

nthPrime :: Int -> Int
nthPrime n = primes !! (n - 1)

isPrime :: Int -> Bool
isPrime n
    | n < 2     = False
    | otherwise = all (\p -> n `mod` p /= 0) (takeWhile (\p -> p * p <= n) primes)