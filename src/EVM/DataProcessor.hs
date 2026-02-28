
module DataProcessor where

import Data.List.Split (splitOn)

parseCSV :: String -> [[Double]]
parseCSV content = map (map read . splitOn ",") $ lines content

calculateAverages :: [[Double]] -> [Double]
calculateAverages rows
  | null rows = []
  | otherwise = map avg $ transpose rows
  where
    avg xs = sum xs / fromIntegral (length xs)
    transpose = foldr (zipWith (:)) (repeat [])

processCSVData :: String -> [Double]
processCSVData = calculateAverages . parseCSVmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, 0, 5, -7]
    let result = processNumbers input
    print resultmodule DataProcessor where

import Data.List (tails)

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map avg $ filter (\window -> length window == n) $ tails xs
  where
    avg :: [Double] -> Double
    avg window = sum window / fromIntegral n

smoothData :: Int -> [Double] -> [Double]
smoothData windowSize = movingAverage windowSize

calculateTrend :: [Double] -> Double
calculateTrend values = (last values - head values) / fromIntegral (length values - 1)
module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input = 
    if null input 
    then Left "Empty input"
    else Right $ map parseRow (lines input)
  where
    parseRow line = splitByComma line
    splitByComma = foldr f [""]
      where
        f ',' acc = "":acc
        f c (s:ss) = (c:s):ss
        f _ [] = error "Unexpected empty list"

validateNumericColumn :: CSVData -> Int -> Either String CSVData
validateNumericColumn [] _ = Left "Empty CSV data"
validateNumericColumn rows colIndex
    | colIndex < 0 = Left "Column index cannot be negative"
    | any (\row -> length row <= colIndex) rows = 
        Left $ "Column index " ++ show colIndex ++ " out of bounds"
    | not (all (isNumeric . (!! colIndex)) rows) =
        Left $ "Column " ++ show colIndex ++ " contains non-numeric values"
    | otherwise = Right rows
  where
    isNumeric str = not (null str) && all isDigit str

summarizeColumn :: CSVData -> Int -> Either String Double
summarizeColumn rows colIndex = do
    validated <- validateNumericColumn rows colIndex
    let values = map (read . (!! colIndex)) validated
    return $ sum values / fromIntegral (length values)

formatCSVOutput :: CSVData -> String
formatCSVOutput = intercalate "\n" . map (intercalate ",")

processCSVData :: String -> Int -> Either String (String, Double)
processCSVData csvContent colIndex = do
    parsed <- parseCSV csvContent
    avg <- summarizeColumn parsed colIndex
    return (formatCSVOutput parsed, avg)