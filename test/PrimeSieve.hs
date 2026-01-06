
module PrimeSieve where

sieve :: Int -> [Int]
sieve limit
    | limit < 2 = []
    | otherwise = go [2..limit] []
  where
    go [] primes = reverse primes
    go (x:xs) primes = go (filter (\n -> n `mod` x /= 0) xs) (x:primes)

primesUpTo :: Int -> [Int]
primesUpTo = sieve