module PrimeSieve where

primesUpTo :: Int -> [Int]
primesUpTo n
    | n < 2     = []
    | otherwise = sieve [2..n]
    where
        sieve [] = []
        sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]module PrimeSieve where

sieve :: Int -> [Int]
sieve limit
    | limit < 2 = []
    | otherwise = go [2..limit] []
  where
    go []     primes = reverse primes
    go (x:xs) primes = go (filter (\n -> n `mod` x /= 0) xs) (x:primes)

primesUpTo :: Int -> [Int]
primesUpTo = sieve

main :: IO ()
main = do
    putStrLn "Enter limit for prime generation:"
    input <- getLine
    let limit = read input :: Int
    let primes = primesUpTo limit
    putStrLn $ "Primes up to " ++ show limit ++ ": " ++ show primes