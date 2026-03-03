module PrimeSieve where

sieve :: Int -> [Int]
sieve limit
    | limit < 2 = []
    | otherwise = sieveHelper [2..limit] []

sieveHelper :: [Int] -> [Int] -> [Int]
sieveHelper []     primes = reverse primes
sieveHelper (p:xs) primes = sieveHelper (filter (\x -> x `mod` p /= 0) xs) (p:primes)

primesUpTo :: Int -> [Int]
primesUpTo = sieve