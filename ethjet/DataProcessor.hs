
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 10) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> 0) xs then Just xs else Nothing

main :: IO ()
main = do
    let sampleData = [5, 12, 8, 15, 3, 20]
    case validateInput sampleData of
        Just validData -> do
            putStrLn "Original data:"
            print validData
            putStrLn "Processed data (values > 10 doubled):"
            print $ processData validData
        Nothing -> putStrLn "Invalid input: all values must be positive"module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Bool
validateInput = all (\x -> x >= -100 && x <= 100)

safeProcessData :: [Int] -> Maybe [Int]
safeProcessData xs
    | validateInput xs = Just (processData xs)
    | otherwise = Nothing