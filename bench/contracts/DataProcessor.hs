module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedData :: (Int -> Bool) -> (Int -> Int) -> [Int] -> Int
sumProcessedData predicate transformer = sum . filterAndTransform predicate transformer

main :: IO ()
main = do
    let sampleData = [1..10]
    putStrLn "Original data:"
    print sampleData
    putStrLn "\nSquares of even numbers:"
    print $ processEvenSquares sampleData
    putStrLn "\nSum of squares of even numbers:"
    print $ sumProcessedData even (\x -> x * x) sampleData
module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type Row = [String]
type CSVData = [Row]

validateRow :: Row -> Either String Row
validateRow [] = Left "Empty row"
validateRow row
    | length row /= 3 = Left $ "Expected 3 columns, got " ++ show (length row)
    | not (all validNumber [row !! 1, row !! 2]) = Left "Invalid numeric values"
    | otherwise = Right row
  where
    validNumber str = not (null str) && all isDigit str && read str > 0

parseCSV :: String -> Either String CSVData
parseCSV content = 
    let rows = map (splitOn ',') (lines content)
        validated = mapM validateRow rows
    in case validated of
        Left err -> Left $ "CSV validation failed: " ++ err
        Right dataRows -> Right dataRows

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr splitHelper [""]
  where
    splitHelper char acc
        | char == delimiter = "":acc
        | otherwise = (char:head acc):tail acc

calculateTotal :: CSVData -> Either String Double
calculateTotal rows = 
    let values = concatMap (take 2 . drop 1) rows
        numbers = map read values :: [Double]
    in Right (sum numbers)

processCSVData :: String -> Either String (CSVData, Double)
processCSVData input = do
    parsed <- parseCSV input
    total <- calculateTotal parsed
    return (parsed, total)module DataProcessor where

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

calculateTrend :: Fractional a => [a] -> Maybe a
calculateTrend [] = Nothing
calculateTrend values =
    let n = fromIntegral $ length values
        indices = [0..n-1]
        sumX = sum indices
        sumY = sum values
        sumXY = sum $ zipWith (*) indices values
        sumX2 = sum $ map (^2) indices
        slope = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX)
    in Just slopemodule DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage _ [] = []
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | n > length xs = []
    | otherwise = map avg $ windows n xs
  where
    windows m ys = take (length ys - m + 1) $ zipWith (++) (tails ys) (repeat [])
    avg zs = sum zs / fromIntegral (length zs)

-- Helper function similar to Data.List.tails but returns lists of exact length
tails :: [a] -> [[a]]
tails [] = []
tails xs@(_:ys) = take n (iterate init xs)
  where n = length xs
module DataProcessor where

import Data.List.Split (splitOn)

parseCSV :: String -> [[Double]]
parseCSV csvData = map (map read . splitOn ",") (lines csvData)

calculateAverages :: [[Double]] -> [Double]
calculateAverages rows = map (\col -> sum col / fromIntegral (length col)) (transpose rows)
  where
    transpose :: [[Double]] -> [[Double]]
    transpose [] = []
    transpose ([]:_) = []
    transpose xss = map head xss : transpose (map tail xss)

processCSVData :: String -> [Double]
processCSVData csvData = calculateAverages (parseCSV csvData)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -100) xs then Just xs else Nothingmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 10) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Maybe [Int]
validateInput xs
    | null xs = Nothing
    | any (< 0) xs = Nothing
    | otherwise = Just xsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processData