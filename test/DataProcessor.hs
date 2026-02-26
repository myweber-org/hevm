module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (\x -> x >= -100 && x <= 100) xs
                   then Just xs
                   else Nothing

safeProcess :: [Int] -> Maybe Int
safeProcess xs = do
    valid <- validateInput xs
    return $ sumProcessed valid