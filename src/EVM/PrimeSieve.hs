
module PrimeSieve where

sieve :: Int -> [Int]
sieve limit
    | limit < 2 = []
    | otherwise = sieveHelper [2..limit] []

sieveHelper :: [Int] -> [Int] -> [Int]
sieveHelper [] primes = reverse primes
sieveHelper (x:xs) primes = sieveHelper filtered (x:primes)
    where
        filtered = filter (\n -> n `mod` x /= 0) xs

primesUpTo :: Int -> [Int]
primesUpTo = sieve