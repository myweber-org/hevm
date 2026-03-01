module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter (>0)module DataProcessor where

processNumbers :: [Int] -> [Int]
processNumbers = map (*2) . filter (>0)

sampleData :: [Int]
sampleData = [1, -5, 3, 0, -2, 8]

main :: IO ()
main = do
    let result = processNumbers sampleData
    putStrLn $ "Original list: " ++ show sampleData
    putStrLn $ "Processed list: " ++ show result