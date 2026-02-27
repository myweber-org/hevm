module PrimeSieve where

primesUpTo :: Int -> [Int]
primesUpTo n
    | n < 2     = []
    | otherwise = sieve [2..n]
  where
    sieve [] = []
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

main :: IO ()
main = do
    putStrLn "Enter upper limit:"
    limitStr <- getLine
    let limit = read limitStr :: Int
    let primes = primesUpTo limit
    putStrLn $ "Primes up to " ++ show limit ++ ": " ++ show primesmodule PrimeSieve where

primesUpTo :: Int -> [Int]
primesUpTo n
    | n < 2     = []
    | otherwise = sieve [2..n]
  where
    sieve [] = []
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- Example usage (commented out for clarity):
-- main = print $ primesUpTo 100