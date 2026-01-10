
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (\x -> x * 2 + 1)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (^2)

calculateStats :: [Int] -> (Int, Int, Double)
calculateStats [] = (0, 0, 0.0)
calculateStats xs = (minimum xs, maximum xs, average)
  where
    total = sum xs
    count = length xs
    average = fromIntegral total / fromIntegral count

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | otherwise = null [x | x <- [2..floorSqrt], n `mod` x == 0]
  where
    floorSqrt = floor (sqrt (fromIntegral n))

primesUpTo :: Int -> [Int]
primesUpTo limit = filter isPrime [2..limit]