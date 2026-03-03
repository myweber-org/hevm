module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isAlpha)

type CSVRow = [String]
type ValidationError = String

validateCSVRow :: CSVRow -> Either ValidationError CSVRow
validateCSVRow [] = Left "Empty row"
validateCSVRow row
    | length row < 2 = Left "Row must have at least 2 columns"
    | not (isValidId (head row)) = Left "Invalid ID format"
    | not (isValidName (row !! 1)) = Left "Invalid name format"
    | otherwise = Right row
  where
    isValidId :: String -> Bool
    isValidId = all isDigit

    isValidName :: String -> Bool
    isValidName name = length name >= 2 && all isAlpha name

processCSVData :: [CSVRow] -> Either ValidationError [CSVRow]
processCSVData rows = traverse validateCSVRow rows

formatValidRows :: [CSVRow] -> String
formatValidRows rows = intercalate "\n" (map formatRow rows)
  where
    formatRow :: CSVRow -> String
    formatRow = intercalate ","

safeCSVProcessor :: [CSVRow] -> Either ValidationError String
safeCSVProcessor rows = do
    validated <- processCSVData rows
    return $ formatValidRows validated