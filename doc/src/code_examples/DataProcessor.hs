module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let input = [1..10]
    let result = processData input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
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
    let sampleData = [1,2,3,4,5,6,7,8,9,10]
    case validateInput sampleData of
        Just validData -> do
            putStrLn $ "Original data: " ++ show validData
            putStrLn $ "Processed data: " ++ show (processEvenSquares validData)
            putStrLn $ "Sum of processed data: " ++ show (sumProcessedData validData)
        Nothing -> putStrLn "Invalid input: all numbers must be positive"module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenNumbers :: [Int] -> [Int]
processEvenNumbers = filterAndTransform even (*2)

processOddNumbers :: [Int] -> [Int]
processOddNumbers = filterAndTransform odd (+1)

sumProcessedData :: (Int -> Bool) -> (Int -> Int) -> [Int] -> Int
sumProcessedData predicate transformer =
    sum . filterAndTransform predicate transformer

main :: IO ()
main = do
    let testData = [1..10]
    putStrLn "Original data:"
    print testData
    putStrLn "\nDoubled even numbers:"
    print $ processEvenNumbers testData
    putStrLn "\nIncremented odd numbers:"
    print $ processOddNumbers testData
    putStrLn "\nSum of doubled even numbers:"
    print $ sumProcessedData even (*2) testDatamodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)