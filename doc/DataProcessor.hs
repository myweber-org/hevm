module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type Row = [String]
type CSVData = [Row]

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
                ',' : xs -> splitOnComma xs
                _        -> []

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

validateRow :: Row -> Either String Row
validateRow [] = Left "Row cannot be empty"
validateRow cells
    | any null cells = Left "Empty cell detected"
    | length cells /= expectedColumns = Left $ "Expected " ++ show expectedColumns ++ " columns"
    | not (all isValidNumeric (take 2 cells)) = Left "First two cells must be numeric"
    | otherwise = Right cells
  where
    expectedColumns = 4
    isValidNumeric = all isDigit

processCSV :: String -> Either String CSVData
processCSV input = do
    parsed <- parseCSV input
    mapM validateRow parsed

formatOutput :: CSVData -> String
formatOutput rows = 
    intercalate "\n" (map formatRow rows)
  where
    formatRow cells = intercalate " | " cells

main :: IO ()
main = do
    let sampleData = "123,456,Product A,Active\n789,012,Product B,Inactive"
    case processCSV sampleData of
        Left err -> putStrLn $ "Error: " ++ err
        Right data' -> putStrLn $ "Processed data:\n" ++ formatOutput data'