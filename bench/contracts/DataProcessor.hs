module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedData :: (Int -> Bool) -> (Int -> Int) -> [Int] -> Int
sumProcessedData predicate transformer = sum . filterAndTransform predicate transformer

main :: IO ()
main = do
    let sampleData = [1..10]
    putStrLn "Original data:"
    print sampleData
    putStrLn "\nSquares of even numbers:"
    print $ processEvenSquares sampleData
    putStrLn "\nSum of squares of even numbers:"
    print $ sumProcessedData even (\x -> x * x) sampleData
module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type Row = [String]
type CSVData = [Row]

validateRow :: Row -> Either String Row
validateRow [] = Left "Empty row"
validateRow row
    | length row /= 3 = Left $ "Expected 3 columns, got " ++ show (length row)
    | not (all validNumber [row !! 1, row !! 2]) = Left "Invalid numeric values"
    | otherwise = Right row
  where
    validNumber str = not (null str) && all isDigit str && read str > 0

parseCSV :: String -> Either String CSVData
parseCSV content = 
    let rows = map (splitOn ',') (lines content)
        validated = mapM validateRow rows
    in case validated of
        Left err -> Left $ "CSV validation failed: " ++ err
        Right dataRows -> Right dataRows

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr splitHelper [""]
  where
    splitHelper char acc
        | char == delimiter = "":acc
        | otherwise = (char:head acc):tail acc

calculateTotal :: CSVData -> Either String Double
calculateTotal rows = 
    let values = concatMap (take 2 . drop 1) rows
        numbers = map read values :: [Double]
    in Right (sum numbers)

processCSVData :: String -> Either String (CSVData, Double)
processCSVData input = do
    parsed <- parseCSV input
    total <- calculateTotal parsed
    return (parsed, total)