module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isAlpha)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> CSVData
parseCSV = map (splitBy ',') . lines
  where
    splitBy :: Char -> String -> [String]
    splitBy delimiter = foldr splitter [[]]
      where
        splitter :: Char -> [String] -> [String]
        splitter c (current:rest)
          | c == delimiter = []:current:rest
          | otherwise = (c:current):rest

validateRow :: CSVRow -> Bool
validateRow row = length row >= 2 && all validField row
  where
    validField :: String -> Bool
    validField field = not (null field) && all validChar field
    
    validChar :: Char -> Bool
    validChar c = isAlpha c || isDigit c || c `elem` ".-_ "

processCSV :: String -> Either String CSVData
processCSV input
  | null input = Left "Empty input"
  | otherwise = 
      let parsed = parseCSV input
          validRows = filter validateRow parsed
      in if length validRows == length parsed
         then Right parsed
         else Left "Invalid data format detected"

formatOutput :: CSVData -> String
formatOutput = intercalate "\n" . map (intercalate " | ")