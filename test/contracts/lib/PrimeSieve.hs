module PrimeSieve where

primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- Function to get the first n prime numbers
firstNPrimes :: Int -> [Int]
firstNPrimes n = take n primes

-- Example usage in main
main :: IO ()
main = do
    putStrLn "First 20 prime numbers:"
    print (firstNPrimes 20)