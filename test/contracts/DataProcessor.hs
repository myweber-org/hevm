
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
    return (formatCSVOutput parsed, avg)