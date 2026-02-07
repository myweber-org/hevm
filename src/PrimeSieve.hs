module PrimeSieve where

sieve :: Int -> [Int]
sieve limit
    | limit < 2 = []
    | otherwise = sieve' [2..limit] []
    where
        sieve' [] primes = reverse primes
        sieve' (p:xs) primes = sieve' (filter (\x -> x `mod` p /= 0) xs) (p:primes)

primesUpTo :: Int -> [Int]
primesUpTo = sieve

main :: IO ()
main = do
    putStrLn "Enter limit:"
    input <- getLine
    let limit = read input :: Int
    print $ primesUpTo limitmodule PrimeSieve where

primesUpTo :: Int -> [Int]
primesUpTo n
    | n < 2     = []
    | otherwise = sieve [2..n]
  where
    sieve [] = []
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]module PrimeSieve where

sieve :: [Int] -> [Int]
sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primes :: [Int]
primes = sieve [2..]

getPrimesUpTo :: Int -> [Int]
getPrimesUpTo n = takeWhile (<= n) primesmodule PrimeSieve where

primesUpTo :: Int -> [Int]
primesUpTo n
    | n < 2     = []
    | otherwise = sieve [2..n]
  where
    sieve [] = []
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]