
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData = all (> 0)

main :: IO ()
main = do
    let sampleData = [-3, 2, 0, 7, -1, 4]
    putStrLn $ "Original data: " ++ show sampleData
    
    let processed = processData sampleData
    putStrLn $ "Processed data: " ++ show processed
    
    putStrLn $ "Data validation: " ++ show (validateData processed)