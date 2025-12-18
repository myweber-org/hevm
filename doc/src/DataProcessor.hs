
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

processOddCubes :: [Int] -> [Int]
processOddCubes = filterAndTransform odd (\x -> x * x * x)

validateAndProcess :: [Int] -> Maybe [Int]
validateAndProcess xs
    | null xs = Nothing
    | any (< 0) xs = Nothing
    | otherwise = Just (processEvenSquares xs ++ processOddCubes xs)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput xs = 
    if all (\x -> x >= -100 && x <= 100) xs
        then Just xs
        else Nothing

main :: IO ()
main = do
    let sampleData = [-5, 2, 0, 8, -3, 10]
    case validateInput sampleData of
        Just validData -> 
            let result = processData validData
            in putStrLn $ "Processed result: " ++ show result
        Nothing -> 
            putStrLn "Input validation failed: values must be between -100 and 100"
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedData :: (Int -> Bool) -> (Int -> Int) -> [Int] -> Int
sumProcessedData predicate transformer = sum . filterAndTransform predicate transformer

main :: IO ()
main = do
    let dataSet = [1..10]
    putStrLn "Original data:"
    print dataSet
    
    putStrLn "\nEven numbers squared:"
    print $ processEvenSquares dataSet
    
    putStrLn "\nSum of squared even numbers:"
    print $ sumProcessedData even (\x -> x * x) dataSet