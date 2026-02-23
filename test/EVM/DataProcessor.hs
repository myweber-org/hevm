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
    let prefix = replicate (windowSize `div` 2) (head dataPoints)
        suffix = replicate (windowSize `div` 2) (last dataPoints)
        extendedData = prefix ++ dataPoints ++ suffix
    in movingAverage windowSize extendedData

calculateTrend :: (Fractional a, Ord a) => [a] -> String
calculateTrend values
    | allIncreasing = "Increasing trend"
    | allDecreasing = "Decreasing trend"
    | otherwise = "No clear trend"
  where
    pairs = zip values (tail values)
    allIncreasing = all (uncurry (<)) pairs
    allDecreasing = all (uncurry (>)) pairs

processDataset :: Fractional a => Int -> [a] -> ([a], String)
processDataset windowSize dataset =
    let smoothed = smoothData windowSize dataset
        trend = calculateTrend smoothed
    in (smoothed, trend)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)
module DataProcessor where

import Data.List.Split (splitOn)

parseCSV :: String -> [[Double]]
parseCSV content = map (map read . splitOn ",") (lines content)

calculateAverages :: [[Double]] -> [Double]
calculateAverages rows = 
    if null rows then []
    else map (\col -> sum col / fromIntegral (length col)) (transpose rows)
  where
    transpose :: [[Double]] -> [[Double]]
    transpose [] = []
    transpose ([]:_) = []
    transpose x = map head x : transpose (map tail x)

processCSVData :: String -> [Double]
processCSVData = calculateAverages . parseCSV

main :: IO ()
main = do
    let csvData = "1.0,2.0,3.0\n4.0,5.0,6.0\n7.0,8.0,9.0"
    let averages = processCSVData csvData
    putStrLn $ "Column averages: " ++ show averages
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 10) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput [] = Nothing
validateInput xs = Just xs

main :: IO ()
main = do
    let sampleData = [5, 12, 8, 20, 3, 15]
    case validateInput sampleData of
        Nothing -> putStrLn "Empty input list"
        Just data' -> do
            let result = processData data'
            putStrLn $ "Original: " ++ show sampleData
            putStrLn $ "Processed: " ++ show resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Bool
validateInput xs = not (null xs) && all (>= -100) xs && all (<= 100) xs

safeProcessData :: [Int] -> Maybe [Int]
safeProcessData xs
    | validateInput xs = Just (processData xs)
    | otherwise = Nothing
module DataProcessor where

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows :: Int -> [a] -> [[a]]
    windows m = takeWhile ((== m) . length) . map (take m) . iterate tail
    
    average :: [Double] -> Double
    average ys = sum ys / fromIntegral (length ys)

safeMovingAverage :: Int -> [Double] -> Maybe [Double]
safeMovingAverage n xs
    | n <= 0 = Nothing
    | otherwise = Just $ movingAverage n xs