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
validateNumericColumn rows colIndex
    | null rows = Left "No data to validate"
    | colIndex < 0 = Left "Column index must be non-negative"
    | otherwise = 
        let header = head rows
            dataRows = tail rows
        in if colIndex >= length header
            then Left "Column index out of bounds"
            else case all (isValidNumericCell colIndex) dataRows of
                True -> Right rows
                False -> Left $ "Non-numeric value found in column " ++ show colIndex
  where
    isValidNumericCell idx row
        | idx >= length row = False
        | otherwise = all isDigit (row !! idx)

calculateColumnAverage :: CSVData -> Int -> Either String Double
calculateColumnAverage rows colIndex = do
    validated <- validateNumericColumn rows colIndex
    let numericValues = map (read . (!! colIndex)) (tail validated)
    return (sum numericValues / fromIntegral (length numericValues))

formatCSVOutput :: CSVData -> String
formatCSVOutput = intercalate "\n" . map (intercalate ",")