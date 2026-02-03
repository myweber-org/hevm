
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
    parseRow :: String -> CSVRow
    parseRow = splitByComma
    
    splitByComma :: String -> [String]
    splitByComma [] = []
    splitByComma str = 
        let (cell, rest) = break (== ',') str
        in trim cell : case rest of
            [] -> []
            (_:xs) -> splitByComma xs
    
    trim :: String -> String
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

validateNumericColumn :: CSVData -> Int -> Either String CSVData
validateNumericColumn [] _ = Right []
validateNumericColumn rows colIndex
    | colIndex < 0 = Left "Column index cannot be negative"
    | otherwise = 
        case validateAllRows rows of
            [] -> Right rows
            errors -> Left $ "Validation errors:\n" ++ intercalate "\n" errors
  where
    validateAllRows :: CSVData -> [String]
    validateAllRows = concatMap validateRow . zip [1..]
    
    validateRow :: (Int, CSVRow) -> [String]
    validateRow (rowNum, row)
        | colIndex >= length row = 
            ["Row " ++ show rowNum ++ ": Column index out of bounds"]
        | not (all isDigit (filter (/= '.') cell)) = 
            ["Row " ++ show rowNum ++ ": Column " ++ show colIndex ++ 
             " must be numeric, got '" ++ cell ++ "'"]
        | otherwise = []
      where
        cell = row !! colIndex

calculateColumnAverage :: CSVData -> Int -> Either String Double
calculateColumnAverage rows colIndex = do
    validated <- validateNumericColumn rows colIndex
    let numericValues = map (read . (!! colIndex)) validated
    if null numericValues
    then Left "No data to calculate average"
    else Right $ sum numericValues / fromIntegral (length numericValues)

formatCSVOutput :: CSVData -> String
formatCSVOutput = intercalate "\n" . map (intercalate ",")

processCSVData :: String -> Int -> Either String (String, Double)
processCSVData csvString column = do
    parsed <- parseCSV csvString
    avg <- calculateColumnAverage parsed column
    return (formatCSVOutput parsed, avg)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = 
    if all (> -100) xs && length xs <= 1000
    then Just xs
    else Nothing

safeProcess :: [Int] -> Maybe Int
safeProcess = fmap sumProcessed . validateInput
module DataProcessor where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ",") (lines content)

safeReadDouble :: String -> Maybe Double
safeReadDouble s = case reads s of
    [(val, "")] -> Just val
    _ -> Nothing

calculateColumnAverage :: CSVData -> Int -> Maybe Double
calculateColumnAverage rows colIndex
    | null validValues = Nothing
    | otherwise = Just (sum validValues / fromIntegral (length validValues))
  where
    columnValues = mapMaybe (safeGetColumn colIndex) rows
    validValues = mapMaybe safeReadDouble columnValues
    safeGetColumn idx row
        | idx >= 0 && idx < length row = Just (row !! idx)
        | otherwise = Nothing

processCSVFile :: String -> IO (Maybe Double)
processCSVFile filePath = do
    content <- readFile filePath
    let csvData = parseCSV content
    return $ calculateColumnAverage csvData 2

main :: IO ()
main = do
    result <- processCSVFile "data.csv"
    case result of
        Just avg -> putStrLn $ "Average: " ++ show avg
        Nothing -> putStrLn "Could not calculate average"module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, 0, 5, -7]
    let result = processNumbers input
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    print result