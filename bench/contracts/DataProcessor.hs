module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput xs
  | null xs = Nothing
  | otherwise = Just xs

main :: IO ()
main = do
  let sampleData = [-3, 1, 0, 5, -2, 8]
  case validateInput sampleData of
    Nothing -> putStrLn "Empty input list"
    Just data' -> do
      let result = processData data'
      putStrLn $ "Original: " ++ show sampleData
      putStrLn $ "Processed: " ++ show resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

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

normalizeCase :: Transformation
normalizeCase = map toLower
  where toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

processField :: [ValidationRule] -> [Transformation] -> String -> Either String String
processField validations transformations input =
  case validateAll validations input of
    Left err -> Left err
    Right validated -> Right $ applyTransformations transformations validated

validateAll :: [ValidationRule] -> String -> Either String String
validateAll rules input = foldl validateStep (Right input) rules
  where validateStep (Right val) rule = if rule val then Right val else Left "Validation failed"
        validateStep left _ = left

applyTransformations :: [Transformation] -> String -> String
applyTransformations = foldl (flip (.)) id

processCSVRow :: [String] -> Either String [String]
processCSVRow row = sequence $ zipWith processField validationRules transformationRules row
  where validationRules = [validateNonEmpty, validateNumeric, validateNonEmpty]
        transformationRules = [trimWhitespace, id, normalizeCase]

formatOutput :: [String] -> String
formatOutput fields = intercalate "|" fields

main :: IO ()
main = do
  let testData = [" 123 ", "456", "  TEST  "]
  case processCSVRow testData of
    Left err -> putStrLn $ "Error: " ++ err
    Right processed -> putStrLn $ "Processed: " ++ formatOutput processedmodule DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter even