
module DataProcessor where

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

transformUppercase :: Transformation
transformUppercase = map toUpper

transformPadLeft :: Int -> Char -> Transformation
transformPadLeft n c s = replicate (n - length s) c ++ s

processCSVRow :: [ValidationRule] -> [Transformation] -> [String] -> Either String [String]
processCSVRow validators transformers row
    | length row /= length validators = Left "Column count mismatch"
    | not (all id $ zipWith ($) validators row) = Left "Validation failed"
    | otherwise = Right $ zipWith ($) transformers row

formatCSV :: [String] -> String
formatCSV = intercalate ","

safeReadInt :: String -> Maybe Int
safeReadInt s = case reads s of
    [(n, "")] -> Just n
    _ -> Nothing

processDataFile :: [String] -> Either String String
processDataFile rows = do
    processed <- mapM (processCSVRow [validateNumeric, validateAlpha] [transformPadLeft 5 '0', transformUppercase]) rows
    return $ unlines $ map formatCSV processed

main :: IO ()
main = do
    let testData = [["123", "abc"], ["456", "def"]]
    case processDataFile testData of
        Left err -> putStrLn $ "Error: " ++ err
        Right result -> putStrLn resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result