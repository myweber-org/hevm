module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 10) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateData :: [Int] -> Bool
validateData xs = all (> 0) xs && length xs > 0

main :: IO ()
main = do
    let sampleData = [5, 12, 8, 20, 3, 15]
    putStrLn $ "Original data: " ++ show sampleData
    putStrLn $ "Processed data: " ++ show (processData sampleData)
    putStrLn $ "Sum of processed data: " ++ show (sumProcessedData sampleData)
    putStrLn $ "Data validation: " ++ show (validateData sampleData)module DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter (>0)module DataProcessor where

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
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print result