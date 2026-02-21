module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = not (null xs) && all (>= -100) xs && all (<= 100) xs

safeProcess :: [Int] -> Maybe [Int]
safeProcess xs
    | validateInput xs = Just (processData xs)
    | otherwise = Nothingmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 10) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Maybe [Int]
validateInput xs
    | null xs = Nothing
    | any (< 0) xs = Nothing
    | otherwise = Just xs

main :: IO ()
main = do
    let sampleData = [5, 12, 8, 20, 3, 15]
    case validateInput sampleData of
        Just validData -> do
            putStrLn $ "Original data: " ++ show validData
            putStrLn $ "Processed data: " ++ show (processData validData)
            putStrLn $ "Sum of processed data: " ++ show (sumProcessedData validData)
        Nothing -> putStrLn "Invalid input data"