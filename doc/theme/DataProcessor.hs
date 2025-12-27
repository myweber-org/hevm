
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processEvenSquares

validateInput :: [Int] -> Maybe [Int]
validateInput [] = Nothing
validateInput xs = Just xs

main :: IO ()
main = do
    let sampleData = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    case validateInput sampleData of
        Nothing -> putStrLn "No data to process"
        Just data' -> do
            putStrLn $ "Original data: " ++ show data'
            putStrLn $ "Processed data: " ++ show (processEvenSquares data')
            putStrLn $ "Sum of processed data: " ++ show (sumProcessedData data')