module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform even (\x -> x * x + 1)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processNumbers input
    print resultmodule DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter even

sampleData :: [Int]
sampleData = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

main :: IO ()
main = do
    let result = processData sampleData
    putStrLn $ "Processed data: " ++ show result