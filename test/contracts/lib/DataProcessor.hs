module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = all (\x -> x >= -100 && x <= 100) xs

safeProcess :: [Int] -> Maybe [Int]
safeProcess xs
    | validateInput xs = Just (processData xs)
    | otherwise = Nothing
module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input = 
    if null input 
    then Left "Empty input"
    else Right $ map parseRow (lines input)
  where
    parseRow line = splitByComma line
    splitByComma = foldr splitHelper [""]
    splitHelper ',' acc = "":acc
    splitHelper chr (x:xs) = (chr:x):xs

validateNumericColumn :: CSVData -> Int -> Either String CSVData
validateNumericColumn [] _ = Left "No data to validate"
validateNumericColumn rows colIndex
    | colIndex < 0 = Left "Column index cannot be negative"
    | any (\row -> length row <= colIndex) rows = 
        Left $ "Column index " ++ show colIndex ++ " out of bounds"
    | not (all (isNumeric . (!! colIndex)) rows) = 
        Left $ "Column " ++ show colIndex ++ " contains non-numeric values"
    | otherwise = Right rows
  where
    isNumeric str = not (null str) && all isDigit str

formatCSV :: CSVData -> String
formatCSV = intercalate "\n" . map (intercalate ",")

processCSVData :: String -> Int -> Either String String
processCSVData input colIndex = do
    parsed <- parseCSV input
    validated <- validateNumericColumn parsed colIndex
    return $ formatCSV validated
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessed :: (Int -> Int) -> [Int] -> Int
sumProcessed processor = sum . map processor

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Even squares: " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of doubled values: " ++ show (sumProcessed (*2) numbers)