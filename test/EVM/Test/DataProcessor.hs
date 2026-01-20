module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedData :: (Int -> Bool) -> (Int -> Int) -> [Int] -> Int
sumProcessedData predicate transformer = sum . filterAndTransform predicate transformer

validateInput :: [Int] -> Maybe [Int]
validateInput [] = Nothing
validateInput xs = Just xs

main :: IO ()
main = do
    let sampleData = [1..10]
    case validateInput sampleData of
        Nothing -> putStrLn "No data to process"
        Just data' -> do
            putStrLn $ "Original data: " ++ show data'
            putStrLn $ "Even squares: " ++ show (processEvenSquares data')
            putStrLn $ "Sum of even squares: " ++ show (sumProcessedData even (\x -> x * x) data')