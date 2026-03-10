
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
    return (formatCSVOutput parsed, avg)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

main :: IO ()
main = do
    let numbers = [-3, 1, 4, -1, 5, 9, -2, 6]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)
module DataProcessor where

import Data.List (foldl')
import Text.Read (readMaybe)

type Record = (String, Double)

parseCSVLine :: String -> Maybe Record
parseCSVLine line = case words line of
    [name, valStr] -> case readMaybe valStr of
        Just val -> Just (name, val)
        Nothing -> Nothing
    _ -> Nothing

calculateAverage :: [Record] -> Double
calculateAverage records =
    let (total, count) = foldl' (\(s, c) (_, v) -> (s + v, c + 1)) (0, 0) records
    in if count > 0 then total / fromIntegral count else 0

processCSVData :: String -> Maybe Double
processCSVData csvContent = do
    let linesOfContent = lines csvContent
    records <- traverse parseCSVLine linesOfContent
    return $ calculateAverage records

main :: IO ()
main = do
    let sampleData = "Alice 85.5\nBob 92.0\nCharlie 78.5\nDiana 88.0"
    case processCSVData sampleData of
        Just avg -> putStrLn $ "Average: " ++ show avg
        Nothing -> putStrLn "Error processing data"
module DataProcessor where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)

-- | Validates if a string contains only digits
validateDigits :: String -> Bool
validateDigits = all isDigit

-- | Safely parses an integer, returns Nothing on failure
safeParseInt :: String -> Maybe Int
safeParseInt s
    | validateDigits s = Just (read s)
    | otherwise = Nothing

-- | Trims leading and trailing whitespace
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

-- | Transforms a list of strings into a comma-separated string
joinWithCommas :: [String] -> String
joinWithCommas = intercalate ", "

-- | Processes a list of potential number strings, returning valid integers
processNumbers :: [String] -> [Int]
processNumbers = catMaybes . map safeParseInt . map trim

-- | Calculates statistics from a list of numbers
data Stats = Stats
    { total :: Int
    , average :: Double
    , count :: Int
    } deriving (Show, Eq)

calculateStats :: [Int] -> Stats
calculateStats [] = Stats 0 0.0 0
calculateStats xs = Stats total' avg count'
    where
        total' = sum xs
        count' = length xs
        avg = fromIntegral total' / fromIntegral count'

-- | Main processing pipeline
processData :: [String] -> Maybe Stats
processData input
    | null validNumbers = Nothing
    | otherwise = Just (calculateStats validNumbers)
    where
        validNumbers = processNumbers input

-- Example utility for demonstration
exampleUsage :: IO ()
exampleUsage = do
    let rawData = [" 42 ", "invalid", "  123  ", "another", "7"]
    case processData rawData of
        Nothing -> putStrLn "No valid numbers found"
        Just stats -> print statsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)
module DataProcessor where

import Data.Char (isDigit, isAlpha, toUpper)
import Data.List (intercalate)

-- Validate if a string contains only digits
validateNumeric :: String -> Bool
validateNumeric = all isDigit

-- Validate if a string contains only alphabetic characters
validateAlpha :: String -> Bool
validateAlpha = all isAlpha

-- Convert string to uppercase
toUppercase :: String -> String
toUppercase = map toUpper

-- Normalize phone number by removing non-digit characters
normalizePhone :: String -> String
normalizePhone = filter isDigit

-- Format name as "Last, First"
formatName :: String -> String -> String
formatName first last = last ++ ", " ++ first

-- Process a list of strings by applying multiple transformations
processStrings :: (String -> String) -> [String] -> [String]
processStrings f = map f

-- Combine validation and transformation in a pipeline
processUserData :: String -> String -> Either String (String, String)
processUserData name phone
    | not (validateAlpha name) = Left "Invalid name: must contain only letters"
    | not (validateNumeric (normalizePhone phone)) = Left "Invalid phone: must contain digits"
    | otherwise = Right (toUppercase name, normalizePhone phone)

-- Example utility function to demonstrate composition
createUserReport :: String -> String -> String
createUserReport name phone =
    case processUserData name phone of
        Left err -> "Error: " ++ err
        Right (processedName, processedPhone) ->
            "User: " ++ processedName ++ ", Phone: " ++ processedPhonemodule DataProcessor where

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