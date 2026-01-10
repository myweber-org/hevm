
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = all (\x -> x >= -100 && x <= 100) xs

safeProcess :: [Int] -> Maybe [Int]
safeProcess xs
  | validateInput xs = Just $ processData xs
  | otherwise = Nothingmodule DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isAlpha)

type CSVRow = [String]
type ValidationError = String

validateRow :: CSVRow -> Either ValidationError CSVRow
validateRow [] = Left "Empty row"
validateRow row
    | length row /= 3 = Left "Row must have exactly 3 columns"
    | not (all validField row) = Left "Invalid characters in fields"
    | otherwise = Right row
  where
    validField field = not (null field) && all isValidChar field
    isValidChar c = isAlpha c || isDigit c || c `elem` " -_."

parseCSV :: String -> Either ValidationError [CSVRow]
parseCSV input
    | null (lines input) = Left "Empty input"
    | otherwise = traverse validateRow (map splitFields (lines input))
  where
    splitFields = map trim . splitOn ','
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse
    splitOn _ [] = []
    splitOn delimiter str =
        let (token, rest) = break (== delimiter) str
        in token : case rest of
            [] -> []
            (_:xs) -> splitOn delimiter xs

formatOutput :: [CSVRow] -> String
formatOutput rows = intercalate "\n" (map formatRow rows)
  where
    formatRow = intercalate " | " . map padField
    padField field = field ++ replicate (10 - length field) ' '

processCSVData :: String -> Either ValidationError String
processCSVData input = do
    rows <- parseCSV input
    return $ formatOutput rows