
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Bool
validateInput xs = not (null xs) && all (> -100) xs

main :: IO ()
main = do
    let sampleData = [1, -5, 3, 0, 8, -2]
    if validateInput sampleData
        then do
            putStrLn $ "Original data: " ++ show sampleData
            putStrLn $ "Processed data: " ++ show (processData sampleData)
            putStrLn $ "Sum of processed data: " ++ show (sumProcessedData sampleData)
        else putStrLn "Invalid input data"
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbers