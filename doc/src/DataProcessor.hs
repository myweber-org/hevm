module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input = mapM parseRow (lines input)
  where
    parseRow line = case splitOnComma line of
        [] -> Left "Empty row"
        cells -> Right (map trim cells)

    splitOnComma :: String -> [String]
    splitOnComma [] = []
    splitOnComma str = 
        let (cell, rest) = break (== ',') str
        in cell : case rest of
            [] -> []
            (_:xs) -> splitOnComma xs

    trim :: String -> String
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

validateNumericColumn :: CSVData -> Int -> Either String CSVData
validateNumericColumn [] _ = Right []
validateNumericColumn (row:rows) colIndex
    | colIndex < 0 = Left "Column index cannot be negative"
    | colIndex >= length row = Left "Column index out of bounds"
    | all isNumeric (getColumn (row:rows) colIndex) = Right (row:rows)
    | otherwise = Left "Non-numeric value found in specified column"
  where
    getColumn dataRows idx = [row !! idx | row <- dataRows]
    isNumeric = all isDigit

formatCSV :: CSVData -> String
formatCSV = intercalate "\n" . map (intercalate ",")

processCSVFile :: String -> Either String String
processCSVFile content = do
    parsed <- parseCSV content
    validated <- validateNumericColumn parsed 2
    return $ formatCSV validatedmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbers