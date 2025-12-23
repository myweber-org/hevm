module PrimeSieve where

sieve :: Int -> [Int]
sieve limit
    | limit < 2 = []
    | otherwise = sieve' [2..limit] []
  where
    sieve' [] primes = reverse primes
    sieve' (x:xs) primes = sieve' (filter (\n -> n `mod` x /= 0) xs) (x:primes)

main :: IO ()
main = do
    putStrLn "Enter limit for prime generation:"
    input <- getLine
    let limit = read input :: Int
    let primes = sieve limit
    putStrLn $ "Primes up to " ++ show limit ++ ": " ++ show primesmodule PrimeSieve where

primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

nthPrime :: Int -> Int
nthPrime n = primes !! (n - 1)

isPrime :: Int -> Bool
isPrime n
    | n < 2     = False
    | otherwise = all (\p -> n `mod` p /= 0) (takeWhile (\p -> p * p <= n) primes)module PrimeSieve where

sieve :: Int -> [Int]
sieve limit
    | limit < 2 = []
    | otherwise = go [2..limit] []
  where
    go [] primes = reverse primes
    go (x:xs) primes = go (filter (\n -> n `mod` x /= 0) xs) (x:primes)

primesUpTo :: Int -> [Int]
primesUpTo = sieve

main :: IO ()
main = do
    putStrLn "Enter a limit:"
    input <- getLine
    let limit = read input :: Int
    let primes = primesUpTo limit
    putStrLn $ "Primes up to " ++ show limit ++ ": " ++ show primes