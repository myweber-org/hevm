module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 10) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateData :: [Int] -> Bool
validateData xs = all (> 0) xs && length xs > 0

main :: IO ()
main = do
    let sampleData = [5, 12, 8, 20, 3, 15]
    putStrLn $ "Original data: " ++ show sampleData
    putStrLn $ "Processed data: " ++ show (processData sampleData)
    putStrLn $ "Sum of processed data: " ++ show (sumProcessedData sampleData)
    putStrLn $ "Data validation: " ++ show (validateData sampleData)module DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter (>0)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessed :: (Int -> Int) -> [Int] -> Int
sumProcessed processor = sum . map processor

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Even squares: " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of even squares: " ++ show (sumProcessed (\x -> if even x then x*x else 0) numbers)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = all (\x -> x >= -100 && x <= 100) xs

main :: IO ()
main = do
    let sampleData = [-5, 2, 0, 8, -3, 10]
    if validateInput sampleData
        then do
            putStrLn "Processing valid data..."
            let result = processData sampleData
            putStrLn $ "Input: " ++ show sampleData
            putStrLn $ "Result: " ++ show result
        else putStrLn "Invalid input data detected"