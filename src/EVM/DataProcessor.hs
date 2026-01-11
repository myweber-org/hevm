module DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter even

sampleData :: [Int]
sampleData = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

main :: IO ()
main = do
    let result = processData sampleData
    putStrLn $ "Processed data: " ++ show result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (\x -> x >= -100 && x <= 100) xs
                   then Just xs
                   else Nothing

main :: IO ()
main = do
    let sampleData = [-5, 2, 0, 8, -3, 10]
    case validateInput sampleData of
        Just validData -> do
            putStrLn $ "Original data: " ++ show validData
            putStrLn $ "Processed data: " ++ show (processData validData)
        Nothing -> putStrLn "Input validation failed: values must be between -100 and 100"