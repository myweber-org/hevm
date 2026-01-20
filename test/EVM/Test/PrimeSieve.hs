module PrimeSieve where

primesUpTo :: Int -> [Int]
primesUpTo n
    | n < 2     = []
    | otherwise = sieve [2..n]
  where
    sieve []     = []
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]module PrimeSieve where

sieve :: Int -> [Int]
sieve limit
    | limit < 2 = []
    | otherwise = sieveHelper [2..limit] []

sieveHelper :: [Int] -> [Int] -> [Int]
sieveHelper [] primes = reverse primes
sieveHelper (p:xs) primes =
    let filtered = filter (\x -> x `mod` p /= 0) xs
    in sieveHelper filtered (p:primes)

primesUpTo :: Int -> [Int]
primesUpTo = sieve

main :: IO ()
main = do
    putStrLn "Enter limit for prime generation:"
    input <- getLine
    let limit = read input :: Int
    let result = primesUpTo limit
    putStrLn $ "Primes up to " ++ show limit ++ ": " ++ show resultmodule PrimeSieve where

sieve :: Int -> [Int]
sieve limit
    | limit < 2 = []
    | otherwise = sieve' [2..limit] []
  where
    sieve' [] primes = reverse primes
    sieve' (p:xs) primes = sieve' (filter (\x -> x `mod` p /= 0) xs) (p:primes)

main :: IO ()
main = do
    putStrLn "Enter limit for prime generation:"
    input <- getLine
    let limit = read input :: Int
    let primes = sieve limit
    putStrLn $ "Primes up to " ++ show limit ++ ": " ++ show primes