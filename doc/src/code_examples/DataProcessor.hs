module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers
module DataProcessor where

import Data.List (intercalate)
import Data.List.Split (splitOn)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ",") (lines content)

computeColumnAverages :: CSVData -> [Double]
computeColumnAverages [] = []
computeColumnAverages rows@(header:_) = 
    map (computeAverage . map read) (transpose numericRows)
  where
    numericRows = map (filter (all isNumeric)) (tail rows)
    isNumeric str = case reads str :: [(Double, String)] of
        [(_, "")] -> True
        _         -> False

computeAverage :: [Double] -> Double
computeAverage [] = 0.0
computeAverage xs = sum xs / fromIntegral (length xs)

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:xss) = transpose xss
transpose ((x:xs):xss) = 
    (x : [h | (h:_) <- xss]) : transpose (xs : [t | (_:t) <- xss])

formatResults :: [Double] -> String
formatResults averages = 
    "Column averages:\n" ++ 
    intercalate "\n" (zipWith formatLine [1..] averages)
  where
    formatLine idx avg = "Column " ++ show idx ++ ": " ++ show avg

processCSVFile :: String -> IO ()
processCSVFile filename = do
    content <- readFile filename
    let parsed = parseCSV content
    let averages = computeColumnAverages parsed
    putStrLn $ formatResults averagesmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)
module DataProcessor where

import Data.List (transpose)
import Data.List.Split (splitOn)

parseCSV :: String -> Either String [[String]]
parseCSV content =
    let rows = lines content
        parsedRows = map (splitOn ",") rows
        colCounts = map length parsedRows
        uniform = all (== head colCounts) (tail colCounts)
    in if null rows
        then Left "Empty CSV content"
        else if not uniform
            then Left "Rows have inconsistent column counts"
            else Right parsedRows

validateRows :: [[String]] -> [Bool]
validateRows rows =
    let transposed = transpose rows
        columnValidators = map (all (not . null)) transposed
    in columnValidators

processCSVFile :: String -> IO ()
processCSVFile filename = do
    content <- readFile filename
    case parseCSV content of
        Left err -> putStrLn $ "Error: " ++ err
        Right rows -> do
            putStrLn "CSV parsed successfully."
            let valResults = validateRows rows
            if and valResults
                then putStrLn "All columns contain non-empty values."
                else putStrLn "Some columns contain empty values."module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let sampleData = [1..10]
    let result = processData sampleData
    putStrLn $ "Original: " ++ show sampleData
    putStrLn $ "Processed: " ++ show result
module DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter (>0)
module DataProcessor where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)

-- | Validates if a string contains only digits
validateDigits :: String -> Bool
validateDigits = all isDigit

-- | Safely parses an integer from string, returns Nothing on failure
safeParseInt :: String -> Maybe Int
safeParseInt str
    | validateDigits str = Just (read str)
    | otherwise = Nothing

-- | Normalizes whitespace in a string
normalizeWhitespace :: String -> String
normalizeWhitespace = unwords . words

-- | Transforms a list of strings into a comma-separated string
joinWithCommas :: [String] -> String
joinWithCommas = intercalate ", "

-- | Processes a list of potential numeric strings, returning valid integers
processNumericList :: [String] -> [Int]
processNumericList = catMaybes . map safeParseInt

-- | Calculates statistics from a list of integers
calculateStats :: [Int] -> (Int, Int, Double)
calculateStats [] = (0, 0, 0.0)
calculateStats xs = (minimum xs, maximum xs, average)
  where
    average = fromIntegral (sum xs) / fromIntegral (length xs)

-- | Validates email format (simple version)
validateEmail :: String -> Bool
validateEmail email =
    let (local, rest) = break (== '@') email
        (domain, tld) = break (== '.') (drop 1 rest)
    in not (null local) && 
       not (null domain) && 
       not (null tld) && 
       '.' `elem` rest

-- | Cleans user input by trimming and normalizing
cleanUserInput :: String -> String
cleanUserInput = normalizeWhitespace . filter (/= '\r')

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
    let numbers = ["123", "456", "abc", "789"]
    let processed = processNumericList numbers
    let (minVal, maxVal, avgVal) = calculateStats processed
    
    putStrLn $ "Processed numbers: " ++ show processed
    putStrLn $ "Min: " ++ show minVal ++ ", Max: " ++ show maxVal ++ ", Avg: " ++ show avgVal
    putStrLn $ "Email 'test@example.com' valid: " ++ show (validateEmail "test@example.com")
    putStrLn $ "Cleaned input: '" ++ cleanUserInput "  hello   world  " ++ "'"module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

data ValidationError = InvalidRow Int String | MissingField Int String
    deriving (Show, Eq)

type CSVRow = [String]
type ValidatedRow = Either ValidationError CSVRow

validateRow :: Int -> CSVRow -> ValidatedRow
validateRow rowNum fields
    | length fields < 3 = Left $ MissingField rowNum "At least 3 fields required"
    | not (all validNumber [fields !! 0, fields !! 2]) = Left $ InvalidRow rowNum "Numeric fields invalid"
    | otherwise = Right fields
    where
        validNumber str = not (null str) && all isDigit str

parseCSV :: String -> [ValidatedRow]
parseCSV content = zipWith validateRow [1..] rows
    where
        rows = map (splitOn ',') (lines content)
        splitOn :: Char -> String -> [String]
        splitOn delimiter = foldr splitHelper [""]
            where
                splitHelper ch (x:xs)
                    | ch == delimiter = "":x:xs
                    | otherwise = (ch:x):xs

formatResults :: [ValidatedRow] -> String
formatResults rows = intercalate "\n" $ map formatRow rows
    where
        formatRow (Left err) = "ERROR: " ++ show err
        formatRow (Right fields) = "VALID: " ++ intercalate " | " fields

processCSVData :: String -> IO ()
processCSVData input = do
    let results = parseCSV input
    putStrLn $ formatResults results
    let (validCount, errorCount) = countResults results
    putStrLn $ "Valid rows: " ++ show validCount ++ ", Errors: " ++ show errorCount
    where
        countResults = foldr countHelper (0,0)
        countHelper (Left _) (v,e) = (v, e+1)
        countHelper (Right _) (v,e) = (v+1, e)module DataProcessor where

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
    where
        average ys = sum ys / fromIntegral (length ys)
        windows _ [] = []
        windows m ys = take m ys : windows m (tail ys)

-- Example usage
sampleData :: [Double]
sampleData = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]

main :: IO ()
main = do
    putStrLn "Original data:"
    print sampleData
    putStrLn "\nMoving average with window size 3:"
    print $ movingAverage 3 sampleData
    putStrLn "\nMoving average with window size 5:"
    print $ movingAverage 5 sampleData