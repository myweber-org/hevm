module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -100) xs then Just xs else Nothing

safeProcess :: [Int] -> Maybe [Int]
safeProcess xs = do
    validated <- validateInput xs
    return $ processData validatedmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 10) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = 
    if all (> 0) xs 
    then Just xs 
    else Nothing

main :: IO ()
main = do
    let sampleData = [5, 12, 8, 20, 3, 15]
    case validateInput sampleData of
        Just validData -> do
            let result = sumProcessed validData
            putStrLn $ "Sum of processed numbers: " ++ show result
        Nothing -> 
            putStrLn "Invalid input: all numbers must be positive"