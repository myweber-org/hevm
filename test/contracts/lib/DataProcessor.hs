
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processEvenSquares

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (>0) xs then Just xs else Nothing

main :: IO ()
main = do
    let sampleData = [1, 2, 3, 4, 5, 6, 7, 8]
    case validateInput sampleData of
        Just validData -> do
            putStrLn $ "Original data: " ++ show validData
            putStrLn $ "Processed data: " ++ show (processEvenSquares validData)
            putStrLn $ "Sum of processed data: " ++ show (sumProcessedData validData)
        Nothing -> putStrLn "Invalid input: all numbers must be positive"