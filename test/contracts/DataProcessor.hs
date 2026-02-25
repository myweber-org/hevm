module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | length xs < n = []
    | otherwise = avg : movingAverage n (tail xs)
    where
        window = take n xs
        avg = sum window / fromIntegral n

smoothData :: Fractional a => Int -> [a] -> [a]
smoothData windowSize dataPoints =
    let padding = replicate (windowSize `div` 2) (head dataPoints)
        paddedData = padding ++ dataPoints ++ padding
    in movingAverage windowSize paddedData

calculateTrend :: Fractional a => [a] -> a
calculateTrend values =
    let n = fromIntegral (length values)
        indices = [0..n-1]
        sumX = sum indices
        sumY = sum values
        sumXY = sum (zipWith (*) indices values)
        sumX2 = sum (map (^2) indices)
        slope = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX)
    in slopemodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processEvenSquares

main :: IO ()
main = do
    let testData = [1..10]
    putStrLn $ "Original data: " ++ show testData
    putStrLn $ "Processed data: " ++ show (processEvenSquares testData)
    putStrLn $ "Sum of processed data: " ++ show (sumProcessedData testData)module DataProcessor where

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
    let testData = [5, 12, 8, 15, 3, 20]
    if validateData testData
        then do
            putStrLn "Original data:"
            print testData
            putStrLn "Processed data:"
            print $ processData testData
            putStrLn "Sum of processed data:"
            print $ sumProcessedData testData
        else putStrLn "Invalid input data"module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbers