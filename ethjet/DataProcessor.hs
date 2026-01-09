module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

main :: IO ()
main = do
    let numbers = [1, -2, 3, -4, 5]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed numbers: " ++ show (sumProcessed numbers)module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input
    | null input = Left "Empty input"
    | otherwise = Right $ map parseRow (lines input)
  where
    parseRow = splitByComma
    splitByComma [] = []
    splitByComma line = 
        case break (== ',') line of
            (cell, []) -> [cell]
            (cell, _:rest) -> cell : splitByComma rest

validateNumericColumn :: CSVData -> Int -> Either String CSVData
validateNumericColumn [] _ = Right []
validateNumericColumn rows colIndex
    | colIndex < 0 = Left "Column index must be non-negative"
    | otherwise = mapM validateRow rows
  where
    validateRow row
        | colIndex >= length row = Left $ "Row has only " ++ show (length row) ++ " columns"
        | all isDigit (row !! colIndex) = Right row
        | otherwise = Left $ "Non-numeric value in column " ++ show colIndex ++ ": " ++ (row !! colIndex)

formatCSV :: CSVData -> String
formatCSV = intercalate "\n" . map (intercalate ",")

processCSVData :: String -> Int -> Either String String
processCSVData rawData colIndex = do
    parsed <- parseCSV rawData
    validated <- validateNumericColumn parsed colIndex
    return $ formatCSV validated

sampleData :: String
sampleData = "id,name,age\n1,John,25\n2,Jane,30\n3,Bob,abc"

main :: IO ()
main = do
    putStrLn "Original CSV:"
    putStrLn sampleData
    putStrLn "\nProcessing column 2 (age):"
    case processCSVData sampleData 2 of
        Left err -> putStrLn $ "Error: " ++ err
        Right result -> putStrLn result