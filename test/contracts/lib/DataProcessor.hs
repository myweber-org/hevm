
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbersmodule DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter even
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input
    | null input = Left "Empty input"
    | otherwise = Right $ map parseRow (lines input)
  where
    parseRow line = splitOnComma line
    splitOnComma [] = []
    splitOnComma line = 
        let (cell, rest) = break (== ',') line
        in trim cell : case rest of
            [] -> []
            (_:xs) -> splitOnComma xs
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

validateNumericColumn :: CSVData -> Int -> Either String CSVData
validateNumericColumn rows colIndex
    | null rows = Left "No data to validate"
    | colIndex < 0 = Left "Column index must be non-negative"
    | otherwise = 
        let validationResults = map (validateRow colIndex) rows
            errors = [err | Left err <- validationResults]
        in if null errors
            then Right rows
            else Left $ "Validation errors: " ++ intercalate "; " errors
  where
    validateRow idx row
        | idx >= length row = Left $ "Row has only " ++ show (length row) ++ " columns"
        | all isDigit (filter (/= ' ') (row !! idx)) = Right row
        | otherwise = Left $ "Non-numeric value in column " ++ show idx ++ ": " ++ (row !! idx)

processCSVData :: String -> Int -> Either String CSVData
processCSVData input colIndex = do
    parsed <- parseCSV input
    validated <- validateNumericColumn parsed colIndex
    return validated

formatCSVOutput :: CSVData -> String
formatCSVOutput = unlines . map (intercalate ",")