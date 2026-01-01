module DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter (>0)

validateData :: [Int] -> Bool
validateData xs = all (>=0) xs && not (null xs)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    if validateData input
        then print $ processData input
        else putStrLn "Invalid input data"module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (>= -100) xs then Just xs else Nothing

main :: IO ()
main = do
    let sampleData = [1, -5, 3, 0, 8, -2]
    case validateInput sampleData of
        Just validData -> do
            let result = sumProcessedData validData
            putStrLn $ "Sum of processed data: " ++ show result
        Nothing -> putStrLn "Input contains values less than -100"