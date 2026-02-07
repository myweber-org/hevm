module PrimeSieve where

primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

nthPrime :: Int -> Int
nthPrime n = primes !! (n - 1)

main :: IO ()
main = do
    putStrLn "First 20 prime numbers:"
    print $ take 20 primes
    putStrLn "\nThe 100th prime number is:"
    print $ nthPrime 100