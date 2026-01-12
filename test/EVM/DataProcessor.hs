module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)
module DataProcessor where

processEvenSquares :: [Int] -> [Int]
processEvenSquares = map (^2) . filter even

main :: IO ()
main = do
    let input = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    let result = processEvenSquares input
    putStrLn $ "Input list: " ++ show input
    putStrLn $ "Even numbers squared: " ++ show result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, 0, 5, -8]
    let result = processData input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = 
    filterAndTransform (\x -> x > 0 && x `mod` 2 == 0) (\x -> x * 2 + 1)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

main :: IO ()
main = do
    let sampleData = [-5, 2, 3, 8, 10, -1, 7]
    putStrLn $ "Original list: " ++ show sampleData
    putStrLn $ "Processed list: " ++ show (processNumbers sampleData)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed sampleData)