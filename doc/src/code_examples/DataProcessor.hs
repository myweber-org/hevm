module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processNumbers input
    print result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData = all (> 0)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let input = [1..10]
    let result = processData input
    putStrLn $ "Original list: " ++ show input
    putStrLn $ "Processed list: " ++ show result
module DataProcessor where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate)

type ValidationRule = String -> Bool
type Transformation = String -> String

validateNumeric :: ValidationRule
validateNumeric = all isDigit

validateNonEmpty :: ValidationRule
validateNonEmpty = not . all isSpace

trimWhitespace :: Transformation
trimWhitespace = reverse . dropWhile isSpace . reverse . dropWhile isSpace

toUpperCase :: Transformation
toUpperCase = map toUpper
  where
    toUpper c
      | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
      | otherwise = c

processCSVRow :: [ValidationRule] -> [Transformation] -> [String] -> Either String [String]
processCSVRow validators transformers row
  | length row /= length validators = Left "Row length doesn't match validator count"
  | any (== False) validationResults = Left $ "Validation failed: " ++ show validationResults
  | otherwise = Right transformedRow
  where
    validationResults = zipWith ($) validators row
    transformedRow = zipWith ($) transformers row

formatCSV :: [[String]] -> String
formatCSV rows = intercalate "\n" $ map (intercalate ",") rows

safeReadInt :: String -> Maybe Int
safeReadInt s
  | validateNumeric s = Just (read s)
  | otherwise = Nothing

data ProcessingResult = Success [String] | Failure String

processData :: [ValidationRule] -> [Transformation] -> [String] -> ProcessingResult
processData validators transformers row =
  case processCSVRow validators transformers row of
    Left err -> Failure err
    Right result -> Success result