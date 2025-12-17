
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

calculateAverage :: [Int] -> Maybe Double
calculateAverage [] = Nothing
calculateAverage xs = Just (fromIntegral (sum xs) / fromIntegral (length xs))