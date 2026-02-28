module DataProcessor where

import Data.Char (isDigit, toUpper)
import Data.List (intercalate)

-- Validate that a string contains only digits
validateDigits :: String -> Bool
validateDigits = all isDigit

-- Convert a string to uppercase
toUppercase :: String -> String
toUppercase = map toUpper

-- Process a list of strings: validate digits and convert to uppercase
processData :: [String] -> [String]
processData = map toUppercase . filter validateDigits

-- Format processed data as a comma-separated string
formatOutput :: [String] -> String
formatOutput = intercalate ", "

-- Main processing pipeline
processPipeline :: [String] -> String
processPipeline = formatOutput . processData

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
    let inputData = ["123", "abc", "456", "def", "789"]
    let processed = processPipeline inputData
    putStrLn $ "Input: " ++ show inputData
    putStrLn $ "Output: " ++ processedmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = all (\x -> x >= -100 && x <= 100) xs

main :: IO ()
main = do
    let inputData = [1, -2, 3, 0, 5, -8]
    if validateInput inputData
        then do
            let result = processData inputData
            putStrLn $ "Processed data: " ++ show result
        else putStrLn "Invalid input data detected"
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData = all (> 0)

main :: IO ()
main = do
    let inputData = [1, -2, 3, 0, 5, -8]
    let processed = processData inputData
    putStrLn $ "Original data: " ++ show inputData
    putStrLn $ "Processed data: " ++ show processed
    putStrLn $ "Data validation: " ++ show (validateData processed)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processEvenSquares

main :: IO ()
main = do
    let sampleData = [1..10]
    putStrLn $ "Original data: " ++ show sampleData
    putStrLn $ "Processed data: " ++ show (processEvenSquares sampleData)
    putStrLn $ "Sum of processed data: " ++ show (sumProcessedData sampleData)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (* 2)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

import Data.Char (isDigit, toUpper)
import Data.List (intercalate)

data ValidationResult = Valid | Invalid String
    deriving (Show, Eq)

validateEmail :: String -> ValidationResult
validateEmail email
    | '@' `elem` email && '.' `elem` (dropWhile (/= '@') email) = Valid
    | otherwise = Invalid "Invalid email format"

normalizePhone :: String -> String
normalizePhone = filter isDigit

capitalizeWords :: String -> String
capitalizeWords = unwords . map capitalize . words
    where capitalize [] = []
          capitalize (x:xs) = toUpper x : xs

processUserData :: String -> String -> (ValidationResult, String, String)
processUserData email phone =
    let emailResult = validateEmail email
        normalizedPhone = normalizePhone phone
        capitalizedEmail = capitalizeWords email
    in (emailResult, normalizedPhone, capitalizedEmail)

formatOutput :: (ValidationResult, String, String) -> String
formatOutput (valid, phone, email) =
    intercalate "\n" 
        [ "Validation: " ++ show valid
        , "Normalized Phone: " ++ phone
        , "Capitalized Email: " ++ email
        ]

sampleData :: IO ()
sampleData = do
    let result = processUserData "john.doe@example.com" "+1 (234) 567-8900"
    putStrLn $ formatOutput resultmodule DataProcessor where

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
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print result