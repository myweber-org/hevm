module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData = all (> 0) . processData

main :: IO ()
main = do
    let sampleData = [-3, 1, 0, 5, -2, 8]
    putStrLn $ "Original data: " ++ show sampleData
    putStrLn $ "Processed data: " ++ show (processData sampleData)
    putStrLn $ "Validation result: " ++ show (validateData sampleData)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput xs
    | null xs = Nothing
    | otherwise = Just xs

main :: IO ()
main = do
    let sampleData = [-3, 2, 0, 5, -1, 8]
    case validateInput sampleData of
        Nothing -> putStrLn "Empty input list"
        Just data' -> do
            let result = processData data'
            putStrLn $ "Original: " ++ show sampleData
            putStrLn $ "Processed: " ++ show result