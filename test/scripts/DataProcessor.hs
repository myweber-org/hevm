module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (*2)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    print result
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

normalizePhone :: Transformation
normalizePhone = filter isDigit

processCSVRow :: [ValidationRule] -> [Transformation] -> [String] -> Either [String] [String]
processCSVRow validators transformers row
  | null validationErrors = Right transformedRow
  | otherwise = Left validationErrors
  where
    validationErrors = concat $ zipWith validateField row validators
    validateField value validator
      | validator value = []
      | otherwise = ["Validation failed for value: " ++ value]
    transformedRow = zipWith applyTransform row transformers
    applyTransform value transformer = transformer value

formatErrors :: [String] -> String
formatErrors errors = "Errors found:\n" ++ intercalate "\n" errors

safeReadInt :: String -> Maybe Int
safeReadInt str
  | validateNumeric str = Just (read str)
  | otherwise = Nothing

processDataFile :: [String] -> IO ()
processDataFile rows = do
  let results = map (processCSVRow [validateNonEmpty, validateNumeric] [trimWhitespace, normalizePhone]) rows
  mapM_ handleResult results
  where
    handleResult (Left errors) = putStrLn $ formatErrors errors
    handleResult (Right processed) = putStrLn $ "Processed: " ++ show processed
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessed :: (Int -> Int) -> [Int] -> Int
sumProcessed processor = sum . map processor

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Even squares: " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of even squares: " ++ 
        show (sumProcessed (\x -> x * x) (filter even numbers))