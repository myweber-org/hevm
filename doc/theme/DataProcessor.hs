module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (\x -> x * x + 1)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

main :: IO ()
main = do
    let numbers = [1..20]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)
module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input = 
    if null input
        then Left "Empty input"
        else Right $ map parseLine (lines input)
  where
    parseLine = splitByComma
    splitByComma [] = []
    splitByComma line = 
        let (cell, rest) = break (== ',') line
        in trim cell : case rest of
                        ',' : xs -> splitByComma xs
                        _        -> []

validateNumericColumn :: CSVData -> Int -> Either String CSVData
validateNumericColumn [] _ = Right []
validateNumericColumn rows colIndex
    | colIndex < 0 = Left "Column index cannot be negative"
    | otherwise = mapM validateRow rows
  where
    validateRow row
        | colIndex >= length row = Left $ "Row has only " ++ show (length row) ++ " columns"
        | all isDigit (trim (row !! colIndex)) = Right row
        | otherwise = Left $ "Non-numeric value in column " ++ show colIndex ++ ": " ++ row !! colIndex

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

formatCSV :: CSVData -> String
formatCSV = intercalate "\n" . map (intercalate ",")

processCSVData :: String -> Int -> Either String String
processCSVData input colIndex = do
    parsed <- parseCSV input
    validated <- validateNumericColumn parsed colIndex
    return $ formatCSV validated