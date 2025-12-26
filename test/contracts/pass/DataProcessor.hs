
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [1, -2, 3, 0, 5, -8]
    let result = processNumbers numbers
    print result
module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isAlpha)

data ValidationError = InvalidFormat String
                     | MissingField String
                     | InvalidValue String String
                     deriving (Show, Eq)

type CSVRow = [String]
type ValidationResult = Either ValidationError CSVRow

validateCSVRow :: CSVRow -> ValidationResult
validateCSVRow row
    | length row < 3 = Left $ MissingField "Row must contain at least 3 fields"
    | not (all validField row) = Left $ InvalidFormat "Fields contain invalid characters"
    | not (validAge (row !! 1)) = Left $ InvalidValue "age" (row !! 1)
    | otherwise = Right row
  where
    validField field = not (null field) && all (\c -> isAlpha c || isDigit c || c `elem` " -") field
    validAge ageStr = case reads ageStr :: [(Int, String)] of
        [(age, "")] -> age >= 0 && age <= 150
        _ -> False

processCSVData :: [CSVRow] -> ([CSVRow], [ValidationError])
processCSVData rows = foldr processRow ([], []) rows
  where
    processRow row (validRows, errors) =
        case validateCSVRow row of
            Left err -> (validRows, err : errors)
            Right validRow -> (validRow : validRows, errors)

formatValidationReport :: ([CSVRow], [ValidationError]) -> String
formatValidationReport (validRows, errors) =
    "CSV Processing Report\n" ++
    "====================\n" ++
    "Valid rows: " ++ show (length validRows) ++ "\n" ++
    "Errors: " ++ show (length errors) ++ "\n" ++
    if null errors
        then "No validation errors found.\n"
        else "Error details:\n" ++ intercalate "\n" (map show errors) ++ "\n"

sampleData :: [CSVRow]
sampleData =
    [ ["John Doe", "30", "Engineer"]
    , ["Jane Smith", "25", "Designer"]
    , ["Bob", "invalid", "Manager"]
    , ["Alice", "200", "Developer"]
    , ["", "40", "Analyst"]
    ]

main :: IO ()
main = do
    let (validRows, errors) = processCSVData sampleData
    putStrLn $ formatValidationReport (validRows, errors)
    putStrLn "Valid rows:"
    mapM_ print validRows
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processDatamodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processNumbers input
    print result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedList :: (Int -> Bool) -> (Int -> Int) -> [Int] -> Int
sumProcessedList predicate transformer = 
    sum . filterAndTransform predicate transformer

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn "Original list:"
    print numbers
    putStrLn "Even numbers squared:"
    print $ processEvenSquares numbers
    putStrLn "Sum of even squares:"
    print $ sumProcessedList even (\x -> x * x) numbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (^2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processEvenSquares

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Even squares: " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of even squares: " ++ show (sumProcessed numbers)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processEvenSquares

main :: IO ()
main = do
    let sampleData = [1..10]
    putStrLn $ "Original data: " ++ show sampleData
    putStrLn $ "Processed data: " ++ show (processEvenSquares sampleData)
    putStrLn $ "Sum of processed data: " ++ show (sumProcessedData sampleData)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (\x -> x * x + 1)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)
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

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input = if null input
                 then Left "Empty input"
                 else Right $ map parseRow (lines input)
  where
    parseRow :: String -> CSVRow
    parseRow = splitByComma

    splitByComma :: String -> [String]
    splitByComma [] = []
    splitByComma str = 
        let (cell, rest) = break (== ',') str
        in trim cell : case rest of
                        ',' : xs -> splitByComma xs
                        _        -> []

    trim :: String -> String
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

validateNumericColumn :: CSVData -> Int -> Either String CSVData
validateNumericColumn [] _ = Left "Empty data"
validateNumericColumn rows colIndex
    | colIndex < 0 = Left "Column index cannot be negative"
    | any (\row -> colIndex >= length row) rows = Left "Column index out of bounds"
    | not (all (isNumeric . (!! colIndex)) rows) = Left "Column contains non-numeric values"
    | otherwise = Right rows
  where
    isNumeric :: String -> Bool
    isNumeric = all (\c -> isDigit c || c == '.')

calculateColumnAverage :: CSVData -> Int -> Either String Double
calculateColumnAverage rows colIndex = do
    validated <- validateNumericColumn rows colIndex
    let values = map (read . (!! colIndex)) validated
    return (sum values / fromIntegral (length values))

formatCSVOutput :: CSVData -> String
formatCSVOutput = intercalate "\n" . map (intercalate ",")
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = all (\x -> x >= -100 && x <= 100) xs

safeProcess :: [Int] -> Maybe [Int]
safeProcess xs
  | validateInput xs = Just (processData xs)
  | otherwise = Nothingmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [-3, 2, 0, 5, -1, 8]
    let result = processData input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result