
module DataProcessor where

import Data.Char (isAlpha, isDigit, toLower)

-- Validate username: only alphanumeric, 3-20 chars
validateUsername :: String -> Bool
validateUsername username =
    let len = length username
        validChars = all (\c -> isAlpha c || isDigit c) username
    in len >= 3 && len <= 20 && validChars

-- Normalize email: lowercase and trim whitespace
normalizeEmail :: String -> String
normalizeEmail = filter (/= ' ') . map toLower

-- Transform phone number to international format
formatPhone :: String -> String
formatPhone phone =
    case filter isDigit phone of
        digits | length digits == 10 -> "+1" ++ digits
               | length digits == 11 && head digits == '1' -> "+" ++ digits
               | otherwise -> phone

-- Process user data with validation
processUserData :: String -> String -> String -> Maybe (String, String, String)
processUserData username email phone =
    if validateUsername username
        then Just (username, normalizeEmail email, formatPhone phone)
        else Nothing

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
    let result = processUserData "john_doe123" "JOHN@EXAMPLE.COM " "555-123-4567"
    case result of
        Just (user, mail, tel) -> 
            putStrLn $ "Processed: " ++ user ++ ", " ++ mail ++ ", " ++ tel
        Nothing -> 
            putStrLn "Invalid username provided"module DataProcessor where

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

validateData :: [Int] -> Bool
validateData xs = all (> 0) xs && length xs > 0

main :: IO ()
main = do
    let sampleData = [1, -2, 3, 0, 5, -8]
    let processed = processData sampleData
    putStrLn $ "Original data: " ++ show sampleData
    putStrLn $ "Processed data: " ++ show processed
    putStrLn $ "Data valid: " ++ show (validateData processed)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processDatamodule DataProcessor where

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

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processNumbers input
    print result