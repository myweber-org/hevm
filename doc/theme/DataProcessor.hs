
module DataProcessor where

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
    | otherwise = Right $ zipWith ($) transformers processedRow
  where
    processedRow = map trimWhitespace row
    validationResults = zipWith ($) validators processedRow
    formatErrors = intercalate ", " . map (\(i, r) -> "col" ++ show i ++ ": " ++ if r then "pass" else "fail") . zip [1..]

safeReadDouble :: String -> Maybe Double
safeReadDouble s = case reads s of
    [(num, "")] -> Just num
    _ -> Nothing

normalizePhoneNumber :: Transformation
normalizePhoneNumber = filter isDigitmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData xs = all (> 0) xs && length xs > 0

main :: IO ()
main = do
    let inputData = [1, -2, 3, 0, 5, -8]
    let processed = processData inputData
    putStrLn $ "Input: " ++ show inputData
    putStrLn $ "Processed: " ++ show processed
    putStrLn $ "Validation: " ++ show (validateData processed)