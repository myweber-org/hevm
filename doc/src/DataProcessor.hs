
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData = all (> 0)

main :: IO ()
main = do
    let inputData = [1, -2, 3, 0, 5, -8]
    let processed = processData inputData
    putStrLn $ "Original data: " ++ show inputData
    putStrLn $ "Processed data: " ++ show processed
    putStrLn $ "Data validation: " ++ show (validateData processed)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let input = [1..10]
    let result = processData input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show resultmodule DataProcessor where

import Data.List.Split (splitOn)

parseCSV :: String -> [[Double]]
parseCSV content = map (map read . splitOn ",") $ lines content

calculateAverages :: [[Double]] -> [Double]
calculateAverages rows
    | null rows = []
    | otherwise = map avg $ transpose rows
  where
    avg xs = sum xs / fromIntegral (length xs)
    transpose = foldr (zipWith (:)) (repeat [])

processCSVData :: String -> [Double]
processCSVData = calculateAverages . parseCSV

main :: IO ()
main = do
    let csvData = "1.0,2.0,3.0\n4.0,5.0,6.0\n7.0,8.0,9.0"
    let averages = processCSVData csvData
    putStrLn $ "Column averages: " ++ show averagesmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processDatamodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (^2)

sumProcessedData :: (Int -> Bool) -> (Int -> Int) -> [Int] -> Int
sumProcessedData predicate transformer = 
    sum . filterAndTransform predicate transformer

main :: IO ()
main = do
    let testData = [1..10]
    putStrLn $ "Original data: " ++ show testData
    putStrLn $ "Even squares: " ++ show (processEvenSquares testData)
    putStrLn $ "Sum of even squares: " ++ show (sumProcessedData even (^2) testData)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processEvenSquares

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input
    | null input = Right []
    | otherwise = mapM parseRow (lines input)
  where
    parseRow line = case splitOnComma line of
        [] -> Left "Empty row"
        cells -> Right cells

splitOnComma :: String -> [String]
splitOnComma = foldr splitHelper [""]
  where
    splitHelper ',' (current:rest) = "":current:rest
    splitHelper char (current:rest) = (char:current):rest

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

processCSVFile :: String -> Either String String
processCSVFile content = do
    parsed <- parseCSV content
    validated <- validateNumericColumn parsed 1
    return $ formatCSV validated