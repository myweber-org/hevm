
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedList :: (Int -> Bool) -> (Int -> Int) -> [Int] -> Int
sumProcessedList predicate transformer = sum . filterAndTransform predicate transformer

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Even squares: " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of even squares: " ++ show (sumProcessedList even (\x -> x * x) numbers)module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input = mapM parseRow (lines input)
  where
    parseRow line = case splitOnComma line of
        [] -> Left "Empty row"
        fields -> Right fields

splitOnComma :: String -> [String]
splitOnComma = foldr splitHelper [""]
  where
    splitHelper ',' (current:rest) = "":current:rest
    splitHelper char (current:rest) = (char:current):rest

validateNumericField :: String -> Either String Int
validateNumericField field
    | all isDigit field = Right (read field)
    | otherwise = Left $ "Non-numeric value: " ++ field

processCSVData :: CSVData -> Either String [(String, Int)]
processCSVData rows = case rows of
    [] -> Left "No data provided"
    header:dataRows -> mapM processRow dataRows
  where
    processRow [name, valueStr] = do
        value <- validateNumericField valueStr
        return (name, value)
    processRow _ = Left "Invalid row format"

formatResults :: [(String, Int)] -> String
formatResults results = intercalate "\n" $
    "Processed Results:" : map (\(name, val) -> name ++ ": " ++ show val) results

main :: IO ()
main = do
    let csvContent = "Name,Value\nAlice,25\nBob,30\nCharlie,xyz"
    case parseCSV csvContent >>= processCSVData of
        Left err -> putStrLn $ "Error: " ++ err
        Right results -> putStrLn $ formatResults resultsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print result