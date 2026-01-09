
module PrimeSieve where

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
  print $ sieve limitmodule PrimeSieve where

sieve :: Int -> [Int]
sieve limit
    | limit < 2 = []
    | otherwise = go [2..limit] []
    where
        go []     acc = reverse acc
        go (x:xs) acc = go (filter (\n -> n `mod` x /= 0) xs) (x:acc)

primesUpTo :: Int -> [Int]
primesUpTo = sieve

main :: IO ()
main = do
    putStrLn "Enter limit:"
    input <- getLine
    let limit = read input :: Int
    print $ primesUpTo limitmodule PrimeSieve where

sieve :: Int -> [Int]
sieve limit
    | limit < 2 = []
    | otherwise = sieve' [2..limit] []
    where
        sieve' [] primes = reverse primes
        sieve' (x:xs) primes = sieve' (filter (\n -> n `mod` x /= 0) xs) (x:primes)

main :: IO ()
main = do
    putStrLn "Enter limit for prime numbers:"
    input <- getLine
    let limit = read input :: Int
    let primes = sieve limit
    putStrLn $ "Prime numbers up to " ++ show limit ++ ":"
    print primesmodule PrimeSieve where

sieve :: Int -> [Int]
sieve limit
    | limit < 2 = []
    | otherwise = sieve' [2..limit] []
    where
        sieve' [] primes = reverse primes
        sieve' (p:xs) primes = sieve' (filter (\x -> x `mod` p /= 0) xs) (p:primes)

primesUpTo :: Int -> [Int]
primesUpTo = sieve

main :: IO ()
main = do
    putStrLn "Enter limit:"
    input <- getLine
    let limit = read input :: Int
    print $ primesUpTo limitmodule PrimeSieve where

sieve :: Int -> [Int]
sieve limit
    | limit < 2 = []
    | otherwise = go [2..limit] []
    where
        go []     acc = reverse acc
        go (x:xs) acc = go (filter (\n -> n `mod` x /= 0) xs) (x:acc)

primesUpTo :: Int -> [Int]
primesUpTo = sieve

main :: IO ()
main = do
    putStrLn "Enter limit:"
    input <- getLine
    let limit = read input :: Int
    print $ primesUpTo limit
module PrimeSieve where

sieve :: Int -> [Int]
sieve limit
    | limit < 2 = []
    | otherwise = go [2..limit] []
  where
    go []     acc = reverse acc
    go (p:xs) acc = go (filter (\x -> x `mod` p /= 0) xs) (p:acc)

primesUpTo :: Int -> [Int]
primesUpTo = sieve