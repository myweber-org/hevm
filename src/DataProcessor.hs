
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processNumbers input
    putStrLn $ "Original list: " ++ show input
    putStrLn $ "Processed list: " ++ show resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)
module DataProcessor where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate)

type ValidationRule = String -> Bool
type Transformation = String -> String

validateNumeric :: ValidationRule
validateNumeric = all (\c -> isDigit c || c == '.')

validateNonEmpty :: ValidationRule
validateNonEmpty = not . all isSpace

trimWhitespace :: Transformation
trimWhitespace = reverse . dropWhile isSpace . reverse . dropWhile isSpace

toUpperCase :: Transformation
toUpperCase = map toUpper

processCSVRow :: [ValidationRule] -> [Transformation] -> [String] -> Either String [String]
processCSVRow validators transformers row
    | length row /= length validators = Left "Column count mismatch"
    | any (== False) validationResults = Left $ "Validation failed: " ++ formatErrors validationResults
    | otherwise = Right transformedRow
  where
    validationResults = zipWith ($) validators row
    transformedRow = zipWith ($) transformers row
    formatErrors = intercalate ", " . map snd . filter (not . fst) . zip validationResults . map (("Column " ++) . show) $ [1..]

safeReadDouble :: String -> Either String Double
safeReadDouble str
    | validateNumeric str = Right (read str)
    | otherwise = Left $ "Invalid numeric format: " ++ str

processNumericColumn :: String -> Either String Double
processNumericColumn = safeReadDouble . trimWhitespacemodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (^2)

sumProcessed :: (Int -> Bool) -> (Int -> Int) -> [Int] -> Int
sumProcessed predicate transformer = 
    sum . filterAndTransform predicate transformermodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData