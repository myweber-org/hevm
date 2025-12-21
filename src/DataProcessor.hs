module DataProcessor where

import Data.List (sort)

type DataPoint = (Int, Double)

filterByThreshold :: Double -> [DataPoint] -> [DataPoint]
filterByThreshold threshold = filter (\(_, value) -> value > threshold)

normalizeData :: [DataPoint] -> [DataPoint]
normalizeData points = map normalize points
  where
    maxVal = maximum $ map snd points
    normalize (id, val) = (id, val / maxVal)

calculateStatistics :: [DataPoint] -> (Double, Double, Double)
calculateStatistics points = (minimum values, maximum values, average values)
  where
    values = map snd points
    average xs = sum xs / fromIntegral (length xs)

sortByValue :: [DataPoint] -> [DataPoint]
sortByValue = sortOn snd

processDataPipeline :: Double -> [DataPoint] -> [DataPoint]
processDataPipeline threshold = 
    sortByValue . normalizeData . filterByThreshold threshold
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processEvenSquaresmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (\x -> x * x + 1)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs
    | null xs = Nothing
    | any (< 0) xs = Nothing
    | otherwise = Just xsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processEvenSquares

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processEvenSquares

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (^2)

calculateStats :: [Int] -> (Int, Int, Double)
calculateStats [] = (0, 0, 0.0)
calculateStats xs = 
    let total = sum xs
        count = length xs
        average = fromIntegral total / fromIntegral count
    in (total, count, average)

main :: IO ()
main = do
    let numbers = [1..10]
    let processed = processEvenSquares numbers
    let (total, count, avg) = calculateStats processed
    
    putStrLn $ "Original numbers: " ++ show numbers
    putStrLn $ "Even squares: " ++ show processed
    putStrLn $ "Total: " ++ show total
    putStrLn $ "Count: " ++ show count
    putStrLn $ "Average: " ++ show avgmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (* 2)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [1, -2, 3, 0, 5, -8]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (^2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processEvenSquares
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transform = map transform . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

main :: IO ()
main = do
    let numbers = [1, -2, 3, 0, 5, -8]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)
    putStrLn $ "First element: " ++ show (safeHead numbers)module DataProcessor where

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map avg $ windows n xs
  where
    windows :: Int -> [a] -> [[a]]
    windows m = takeWhile ((== m) . length) . map (take m) . iterate tail
    
    avg :: [Double] -> Double
    avg ys = sum ys / fromIntegral (length ys)

-- Helper function to test the moving average
testMovingAverage :: IO ()
testMovingAverage = do
    let testData = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
    putStrLn "Testing moving average with window size 3:"
    print $ movingAverage 3 testData
    putStrLn "\nTesting moving average with window size 5:"
    print $ movingAverage 5 testData
module DataProcessor where

import Data.List.Split (splitOn)

parseCSV :: String -> [[Double]]
parseCSV content = map (map read . splitOn ",") (lines content)

calculateAverages :: [[Double]] -> [Double]
calculateAverages rows = 
    if null rows 
    then []
    else map (\col -> sum col / fromIntegral (length col)) (transpose rows)
  where
    transpose :: [[Double]] -> [[Double]]
    transpose [] = []
    transpose ([]:_) = []
    transpose xss = map head xss : transpose (map tail xss)

processCSVData :: String -> [Double]
processCSVData = calculateAverages . parseCSV

validateRowLengths :: [[Double]] -> Bool
validateRowLengths rows = all (== expectedLength) (map length rows)
  where
    expectedLength = if null rows then 0 else length (head rows)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

movingAverage :: Int -> [Double] -> [Double]
movingAverage windowSize xs
    | windowSize <= 0 = error "Window size must be positive"
    | windowSize > length xs = []
    | otherwise = map average $ windows windowSize xs
  where
    windows n = takeWhile ((== n) . length) . map (take n) . tails
    average list = sum list / fromIntegral (length list)