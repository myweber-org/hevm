
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let input = [1..10]
    let result = processData input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let input = [1..10]
    let result = processData input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbers
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (* 2)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    print result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = not (null xs) && all (>= -100) xs && all (<= 100) xs

safeProcess :: [Int] -> Maybe [Int]
safeProcess xs
    | validateInput xs = Just (processData xs)
    | otherwise = Nothing
module DataProcessor where

import Data.List.Split (splitOn)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ",") (lines content)

numericColumns :: CSVData -> [[Double]]
numericColumns [] = []
numericColumns (header:rows) = 
    let indices = [0..length header - 1]
        isNumeric idx = all (isNumericString . (!! idx)) rows
        isNumericString s = case reads s :: [(Double, String)] of
            [(_, "")] -> True
            _ -> False
        numericIndices = filter isNumeric indices
    in map (\idx -> map (read . (!! idx)) rows) numericIndices

columnAverage :: [Double] -> Double
columnAverage xs = sum xs / fromIntegral (length xs)

calculateAverages :: String -> [Double]
calculateAverages csvContent = 
    let data = parseCSV csvContent
        numericData = numericColumns data
    in map columnAverage numericData

processCSVFile :: FilePath -> IO [Double]
processCSVFile path = do
    content <- readFile path
    return $ calculateAverages content
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

-- Process a list of strings with given transformation
processStrings :: (String -> String) -> [String] -> [String]
processStrings f = map f

-- Combine validation and transformation
processWithValidation :: (String -> Bool) -> (String -> String) -> String -> Maybe String
processWithValidation validator transformer input =
    if validator input
    then Just (transformer input)
    else Nothing

-- Example data record
data Person = Person
    { firstName :: String
    , lastName  :: String
    , phone     :: String
    } deriving (Show, Eq)

-- Process person record with validation
processPerson :: Person -> Maybe Person
processPerson person = do
    let validPhone = normalizePhone (phone person)
    if length validPhone >= 10
        then Just person { phone = validPhone }
        else Nothing

-- Convert person to formatted string
personToString :: Person -> String
personToString p = 
    intercalate " | " 
        [ formatName (firstName p) (lastName p)
        , normalizePhone (phone p)
        , toUppercase (firstName p)
        ]

-- Batch process list of persons
processPersonList :: [Person] -> [String]
processPersonList = map personToString . filter (maybe False (const True) . processPerson)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Bool
validateInput xs = not (null xs) && all (> -100) xs

main :: IO ()
main = do
    let sampleData = [1, -5, 3, 0, 8, -2]
    if validateInput sampleData
        then do
            putStrLn $ "Original data: " ++ show sampleData
            putStrLn $ "Processed data: " ++ show (processData sampleData)
            putStrLn $ "Sum of processed data: " ++ show (sumProcessedData sampleData)
        else putStrLn "Invalid input data"