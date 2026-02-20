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