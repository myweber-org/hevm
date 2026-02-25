module PrimeSieve where

primesUpTo :: Int -> [Int]
primesUpTo n
    | n < 2     = []
    | otherwise = sieve [2..n]
    where
        sieve [] = []
        sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
module PrimeSieve where

sieve :: Int -> [Int]
sieve limit
    | limit < 2 = []
    | otherwise = sieve' [2..limit] []
  where
    sieve' [] primes = reverse primes
    sieve' (p:xs) primes =
        let xs' = filter (\x -> x `mod` p /= 0) xs
        in sieve' xs' (p:primes)

primesUpTo :: Int -> [Int]
primesUpTo = sieve

main :: IO ()
main = do
    putStrLn "Enter a limit:"
    input <- getLine
    let limit = read input :: Int
    let primes = primesUpTo limit
    putStrLn $ "Primes up to " ++ show limit ++ ": " ++ show primes