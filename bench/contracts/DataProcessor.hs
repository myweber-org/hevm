module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

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
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isAlpha)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input
    | null input = Left "Empty input"
    | otherwise = Right $ map parseRow (lines input)
  where
    parseRow line = splitOnComma line

splitOnComma :: String -> [String]
splitOnComma [] = []
splitOnComma line = 
    let (field, rest) = break (== ',') line
    in trim field : case rest of
        ',' : xs -> splitOnComma xs
        _        -> []

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

validateRow :: CSVRow -> Either String CSVRow
validateRow row
    | length row < 2 = Left "Row must have at least 2 columns"
    | not (all validField row) = Left "Fields contain invalid characters"
    | otherwise = Right row
  where
    validField field = all (\c -> isAlpha c || isDigit c || c == ' ') field

processCSV :: String -> Either String CSVData
processCSV input = do
    parsed <- parseCSV input
    mapM validateRow parsed

formatOutput :: CSVData -> String
formatOutput rows = 
    intercalate "\n" $ map (intercalate " | ") rows

main :: IO ()
main = do
    let testData = "John Doe,25,Engineer\nJane Smith,30,Designer"
    case processCSV testData of
        Left err -> putStrLn $ "Error: " ++ err
        Right data -> putStrLn $ "Processed data:\n" ++ formatOutput datamodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)