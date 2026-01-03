module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [-3, 1, 0, 5, -2, 8]
    let result = processData input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenNumbers :: [Int] -> [Int]
processEvenNumbers = filterAndTransform even (* 2)

sumProcessedNumbers :: [Int] -> Int
sumProcessedNumbers = sum . processEvenNumbers

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original numbers: " ++ show numbers
    putStrLn $ "Processed numbers: " ++ show (processEvenNumbers numbers)
    putStrLn $ "Sum of processed numbers: " ++ show (sumProcessedNumbers numbers)