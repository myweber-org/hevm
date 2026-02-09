module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

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

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedList :: [Int] -> Int
sumProcessedList = sum . processEvenSquares

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list (even numbers squared): " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of processed list: " ++ show (sumProcessedList numbers)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

main :: IO ()
main = do
    let numbers = [-3, 1, 4, -1, 5, 9, -2, 6]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed numbers: " ++ show (sumProcessed numbers)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs
    | null xs = Nothing
    | any (< -100) xs = Nothing
    | any (> 100) xs = Nothing
    | otherwise = Just xs

safeProcess :: [Int] -> Maybe Int
safeProcess = fmap sumProcessed . validateInput
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

normalizeCase :: Transformation
normalizeCase = map toLower

processCSVRow :: [ValidationRule] -> [Transformation] -> [String] -> Either String [String]
processCSVRow validators transformers row
  | length row /= length validators = Left "Row length mismatch"
  | otherwise = case validateAll validators row of
      Left err -> Left err
      Right _ -> Right $ applyTransformations transformers row

validateAll :: [ValidationRule] -> [String] -> Either String ()
validateAll validators values = 
  case filter (not . snd) $ zip validators values of
    [] -> Right ()
    ((_, val):_) -> Left $ "Validation failed for value: " ++ val

applyTransformations :: [Transformation] -> [String] -> [String]
applyTransformations transformers = zipWith ($) transformers

formatCSVOutput :: [String] -> String
formatCSVOutput = intercalate ","

safeReadDouble :: String -> Maybe Double
safeReadDouble s = 
  case reads s of
    [(val, "")] -> Just val
    _ -> Nothing

dataProcessingPipeline :: [ValidationRule] -> [Transformation] -> [[String]] -> Either String [[String]]
dataProcessingPipeline validators transformers = 
  foldr processRow (Right [])
  where
    processRow row acc = do
      processed <- processCSVRow validators transformers row
      rows <- acc
      return (processed : rows)