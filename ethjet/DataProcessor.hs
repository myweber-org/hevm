
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
            putStrLn "Original data:"
            print sampleData
            putStrLn "Processed data (positive numbers doubled):"
            print $ processData sampleData
        else putStrLn "Input validation failed: values must be between -100 and 100"
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedData :: (Int -> Int) -> [Int] -> Int
sumProcessedData processor = sum . map processor

main :: IO ()
main = do
    let dataSet = [1..10]
    putStrLn "Original data:"
    print dataSet
    
    putStrLn "\nEven numbers squared:"
    let processed = processEvenSquares dataSet
    print processed
    
    putStrLn "\nSum of processed data:"
    print $ sumProcessedData (\x -> x * 2) dataSet