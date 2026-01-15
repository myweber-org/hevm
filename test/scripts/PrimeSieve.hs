
module PrimeSieve where

sieve :: Int -> [Int]
sieve limit
    | limit < 2 = []
    | otherwise = sieve' [2..limit] []
  where
    sieve' [] primes = reverse primes
    sieve' (p:xs) primes =
        let multiples = [p * i | i <- [p..limit `div` p]]
            filtered = filter (`notElem` multiples) xs
        in sieve' filtered (p:primes)

main :: IO ()
main = do
    putStrLn "Enter limit for prime generation:"
    input <- getLine
    let limit = read input :: Int
    let primes = sieve limit
    putStrLn $ "Primes up to " ++ show limit ++ ": " ++ show primes