
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
primesUpTo = sievemodule PrimeSieve where

sieve :: Int -> [Int]
sieve limit
    | limit < 2 = []
    | otherwise = sieveHelper [2..limit] []

sieveHelper :: [Int] -> [Int] -> [Int]
sieveHelper [] primes = reverse primes
sieveHelper (x:xs) primes = sieveHelper (filter (\n -> n `mod` x /= 0) xs) (x:primes)

primesUpTo :: Int -> [Int]
primesUpTo = sieve

main :: IO ()
main = do
    putStrLn "Enter limit for prime numbers:"
    input <- getLine
    let limit = read input :: Int
    let result = primesUpTo limit
    putStrLn $ "Prime numbers up to " ++ show limit ++ ": " ++ show result