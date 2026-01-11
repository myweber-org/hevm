
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (>0) (*2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Bool
validateInput xs = not (null xs) && all (> -100) xs

safeProcess :: [Int] -> Maybe Int
safeProcess xs
    | validateInput xs = Just (sumProcessed xs)
    | otherwise = Nothing