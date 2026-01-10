module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isSpace)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input
    | null (trim input) = Left "Empty input"
    | otherwise = traverse validateRow $ map (splitOn ',') (lines input)
  where
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
    
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitHelper [""]
      where
        splitHelper ch (x:xs)
            | ch == delimiter = "":x:xs
            | otherwise = (ch:x):xs
    
    validateRow :: CSVRow -> Either String CSVRow
    validateRow row
        | length row < 2 = Left $ "Row has insufficient columns: " ++ show row
        | any null (map trim row) = Left $ "Row contains empty fields: " ++ show row
        | not (allDigits (trim (row !! 1))) = Left $ "Second column must be numeric: " ++ show row
        | otherwise = Right (map trim row)
    
    allDigits :: String -> Bool
    allDigits = all isDigit

calculateTotal :: CSVData -> Either String Double
calculateTotal rows = do
    let values = map (read . (!! 1)) rows
    return $ sum values

formatOutput :: CSVData -> String
formatOutput rows = intercalate "\n" $ map (intercalate " | ") rows

processCSVData :: String -> Either String String
processCSVData input = do
    parsed <- parseCSV input
    total <- calculateTotal parsed
    let formatted = formatOutput parsed
    return $ formatted ++ "\nTotal: " ++ show total