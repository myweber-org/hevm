module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData xs = all (> 0) (processData xs)

sampleData :: [Int]
sampleData = [1, -2, 3, 0, 5, -7]

main :: IO ()
main = do
    let result = processData sampleData
    putStrLn $ "Processed data: " ++ show result
    putStrLn $ "Data validation: " ++ show (validateData sampleData)