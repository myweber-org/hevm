
module DataProcessor where

import Data.Char (isDigit, isAlpha, toUpper)
import Data.List (intercalate)

-- Validate if string contains only digits
validateNumeric :: String -> Bool
validateNumeric = all isDigit

-- Validate if string contains only alphabetic characters
validateAlpha :: String -> Bool
validateAlpha = all isAlpha

-- Convert string to standardized uppercase format
standardizeText :: String -> String
standardizeText = map toUpper

-- Process a list of strings with validation and transformation
processData :: [String] -> (Int, Int, [String])
processData items = 
    let numericCount = length $ filter validateNumeric items
        alphaCount = length $ filter validateAlpha items
        processed = map standardizeText items
    in (numericCount, alphaCount, processed)

-- Generate a summary report from processed data
generateReport :: (Int, Int, [String]) -> String
generateReport (numCount, alphaCount, items) =
    "Data Processing Report\n" ++
    "=====================\n" ++
    "Numeric entries: " ++ show numCount ++ "\n" ++
    "Alphabetic entries: " ++ show alphaCount ++ "\n" ++
    "Total entries: " ++ show (length items) ++ "\n" ++
    "Processed items: " ++ intercalate ", " items

-- Main processing pipeline
runDataProcessor :: [String] -> String
runDataProcessor input = 
    let processed = processData input
    in generateReport processed
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 10) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> 0) xs then Just xs else Nothing

main :: IO ()
main = do
    let sampleData = [5, 12, 8, 20, 3, 15]
    case validateInput sampleData of
        Just validData -> do
            putStrLn "Original data:"
            print validData
            putStrLn "Processed data (values > 10 doubled):"
            print $ processData validData
        Nothing -> putStrLn "Invalid input: all values must be positive"module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processDatamodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-5, 3, 0, 8, -2, 10]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers
module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isSpace)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input
    | null input = Left "Empty input"
    | otherwise = Right $ map parseRow $ lines input
  where
    parseRow :: String -> CSVRow
    parseRow = splitByComma . trim
    
    splitByComma :: String -> CSVRow
    splitByComma [] = []
    splitByComma str = 
        let (cell, rest) = break (== ',') str
        in trim cell : if null rest then [] else splitByComma (drop 1 rest)
    
    trim :: String -> String
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

validateNumericColumn :: CSVData -> Int -> Either String CSVData
validateNumericColumn [] _ = Left "Empty data"
validateNumericColumn rows colIndex
    | colIndex < 0 = Left "Column index must be non-negative"
    | any (\row -> length row <= colIndex) rows = 
        Left $ "Column index " ++ show colIndex ++ " out of bounds"
    | not (all (isNumeric . (!! colIndex)) rows) =
        Left $ "Column " ++ show colIndex ++ " contains non-numeric values"
    | otherwise = Right rows
  where
    isNumeric :: String -> Bool
    isNumeric str = not (null str) && all isDigit (filter (/= '.') str)

calculateColumnAverage :: CSVData -> Int -> Either String Double
calculateColumnAverage rows colIndex = do
    validated <- validateNumericColumn rows colIndex
    let values = map (read . (!! colIndex)) validated
    return $ sum values / fromIntegral (length values)

formatCSVOutput :: CSVData -> String
formatCSVOutput = intercalate "\n" . map (intercalate ",")

processCSVData :: String -> Int -> Either String (String, Double)
processCSVData csvString colIndex = do
    parsed <- parseCSV csvString
    avg <- calculateColumnAverage parsed colIndex
    return (formatCSVOutput parsed, avg)