module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedList :: [Int] -> Int
sumProcessedList = sum . processEvenSquares

main :: IO ()
main = do
    let sampleData = [1..10]
    putStrLn $ "Original list: " ++ show sampleData
    putStrLn $ "Processed list (even numbers squared): " ++ show (processEvenSquares sampleData)
    putStrLn $ "Sum of processed list: " ++ show (sumProcessedList sampleData)
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
  | any (== False) validResults = Left $ "Validation failed at columns: " ++ show invalidIndices
  | otherwise = Right transformedRow
  where
    validResults = zipWith ($) validators row
    invalidIndices = map fst $ filter (not . snd) $ zip [0..] validResults
    transformedRow = zipWith ($) transformers row

formatCSV :: [[String]] -> String
formatCSV rows = intercalate "\n" $ map (intercalate ",") rows

safeReadInt :: String -> Maybe Int
safeReadInt s
  | validateNumeric s = Just (read s)
  | otherwise = Nothing

exampleProcessing :: IO ()
exampleProcessing = do
  let sampleData = [["123", "  John  ", "doe"], ["456", "  jane  ", "smith"]]
      validators = [validateNumeric, validateNonEmpty, validateNonEmpty]
      transformers = [id, trimWhitespace, toUpperCase]
  
  putStrLn "Processing CSV data:"
  mapM_ (processRow sampleData) [0..length sampleData - 1]
  
  where
    processRow data idx = case processCSVRow validators transformers (data !! idx) of
      Left err -> putStrLn $ "Row " ++ show idx ++ " error: " ++ err
      Right result -> putStrLn $ "Row " ++ show idx ++ " processed: " ++ show result