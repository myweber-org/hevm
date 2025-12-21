module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

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

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = all (\x -> x >= -100 && x <= 100) xs

safeProcess :: [Int] -> Maybe [Int]
safeProcess xs
    | validateInput xs = Just (processData xs)
    | otherwise = Nothingmodule DataProcessor where

import Data.Char (isDigit, isAlpha)
import Data.List (intercalate)

type Row = [String]
type Table = [Row]

validateRowLength :: Int -> Row -> Bool
validateRowLength expectedLength row = length row == expectedLength

validateNumericField :: String -> Bool
validateNumericField = all isDigit

validateAlphaField :: String -> Bool
validateAlphaField = all isAlpha

transformToUpper :: Row -> Row
transformToUpper = map (map toUpper)

filterValidRows :: (Row -> Bool) -> Table -> Table
filterValidRows validator = filter validator

processCSVData :: Table -> Table
processCSVData = filterValidRows (validateRowLength 3) . map transformToUpper

formatRow :: Row -> String
formatRow = intercalate ","

formatTable :: Table -> String
formatTable = unlines . map formatRow