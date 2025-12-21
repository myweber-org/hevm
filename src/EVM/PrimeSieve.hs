module PrimeSieve where

primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- Function to get the first n prime numbers
firstNPrimes :: Int -> [Integer]
firstNPrimes n = take n primes

-- Example usage in main (not exported)
main :: IO ()
main = do
    putStrLn "First 20 prime numbers:"
    print (firstNPrimes 20)module PrimeSieve where

sieve :: Int -> [Int]
sieve limit
    | limit < 2 = []
    | otherwise = sieve' [2..limit] []
    where
        sieve' [] primes = reverse primes
        sieve' (x:xs) primes = sieve' (filter (\n -> n `mod` x /= 0) xs) (x:primes)

main :: IO ()
main = do
    putStrLn "Enter limit:"
    input <- getLine
    let limit = read input :: Int
    let primes = sieve limit
    putStrLn $ "Primes up to " ++ show limit ++ ": " ++ show primes