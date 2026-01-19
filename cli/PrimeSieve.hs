module PrimeSieve where

primesUpTo :: Int -> [Int]
primesUpTo n
    | n < 2     = []
    | otherwise = sieve [2..n]
    where
        sieve [] = []
        sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- Example usage function
printPrimes :: Int -> IO ()
printPrimes limit = do
    putStrLn $ "Prime numbers up to " ++ show limit ++ ":"
    print (primesUpTo limit)