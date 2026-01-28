
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isAlpha)

type ValidationRule = String -> Bool
type Transformation = String -> String

validateNumeric :: ValidationRule
validateNumeric = all isDigit

validateAlpha :: ValidationRule
validateAlpha = all isAlpha

validateLength :: Int -> ValidationRule
validateLength n s = length s == n

toUpperCase :: Transformation
toUpperCase = map toUpper

toLowerCase :: Transformation
toLowerCase = map toLower

padLeft :: Int -> Char -> Transformation
padLeft n c s = replicate (n - length s) c ++ s

processField :: ValidationRule -> Transformation -> String -> Maybe String
processField validate transform field =
    if validate field
    then Just (transform field)
    else Nothing

processCSVRow :: [ValidationRule] -> [Transformation] -> [String] -> Maybe [String]
processCSVRow validations transformations row
    | length validations /= length row = Nothing
    | length transformations /= length row = Nothing
    | otherwise = sequence $ zipWith3 processField validations transformations row

formatCSVRow :: [String] -> String
formatCSVRow = intercalate ","

validateAndTransformCSV :: [[ValidationRule]] -> [[Transformation]] -> [String] -> Maybe String
validateAndTransformCSV validationsList transformationsList csvRows = do
    processedRows <- sequence $ zipWith3 processCSVRow validationsList transformationsList csvRows
    return $ unlines $ map formatCSVRow processedRows

exampleUsage :: IO ()
exampleUsage = do
    let validations = [[validateNumeric, validateAlpha, validateLength 5]]
    let transformations = [[padLeft 10 '0', toUpperCase, id]]
    let csvData = [["123", "hello", "world"], ["456", "test", "data"]]
    
    case validateAndTransformCSV validations transformations csvData of
        Just result -> putStrLn result
        Nothing -> putStrLn "Validation failed"