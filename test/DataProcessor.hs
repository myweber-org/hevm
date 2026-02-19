module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform isEven square
  where
    isEven n = n `mod` 2 == 0
    square n = n * n

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)
module DataProcessor where

import Data.Char (isDigit, isAlpha, toUpper)
import Data.List (intercalate)

-- Validate if a string contains only digits
validateNumeric :: String -> Bool
validateNumeric = all isDigit

-- Validate if a string contains only alphabetic characters
validateAlpha :: String -> Bool
validateAlpha = all isAlpha

-- Normalize phone number by removing non-digit characters
normalizePhone :: String -> String
normalizePhone = filter isDigit

-- Format name by capitalizing first letter of each word
formatName :: String -> String
formatName = unwords . map capitalize . words
  where
    capitalize [] = []
    capitalize (x:xs) = toUpper x : map toLower xs
    toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

-- Process a list of strings with validation and transformation
processRecords :: [String] -> [Either String String]
processRecords = map processRecord
  where
    processRecord str
      | validateNumeric str = Right $ normalizePhone str
      | validateAlpha str = Right $ formatName str
      | otherwise = Left $ "Invalid record: " ++ str

-- Batch process records and separate valid/invalid results
batchProcess :: [String] -> ([String], [String])
batchProcess records =
  let results = processRecords records
      valid = [r | Right r <- results]
      invalid = [r | Left r <- results]
  in (valid, invalid)

-- Generate report from batch processing results
generateReport :: ([String], [String]) -> String
generateReport (valid, invalid) =
  "Processing Report:\n" ++
  "Valid records: " ++ show (length valid) ++ "\n" ++
  "Invalid records: " ++ show (length invalid) ++ "\n" ++
  "Valid data: " ++ intercalate ", " valid ++ "\n" ++
  "Errors: " ++ intercalate ", " invalidmodule DataProcessor where

import Data.Char (toLower, isAlpha, isSpace)
import Data.List (intercalate)

type Username = String
type Email = String
type UserProfile = (Username, Email, Int)

validateUsername :: Username -> Maybe Username
validateUsername username
    | length username >= 3 && length username <= 20 &&
      all (\c -> isAlpha c || c == '_' || c == '-') username = Just username
    | otherwise = Nothing

normalizeEmail :: Email -> Email
normalizeEmail = map toLower . filter (not . isSpace)

validateEmail :: Email -> Maybe Email
validateEmail email
    | '@' `elem` email && '.' `elem` (dropWhile (/= '@') email) = 
        Just (normalizeEmail email)
    | otherwise = Nothing

createUserProfile :: Username -> Email -> Int -> Maybe UserProfile
createUserProfile username email age = do
    validUsername <- validateUsername username
    validEmail <- validateEmail email
    if age >= 0 && age <= 150
        then Just (validUsername, validEmail, age)
        else Nothing

formatProfile :: UserProfile -> String
formatProfile (username, email, age) =
    intercalate " | " ["User: " ++ username, "Email: " ++ email, "Age: " ++ show age]

processUserData :: [String] -> [String]
processUserData inputs = 
    map processSingle inputs
    where
        processSingle input =
            case words input of
                [username, email, ageStr] ->
                    case reads ageStr of
                        [(age, "")] ->
                            case createUserProfile username email age of
                                Just profile -> formatProfile profile
                                Nothing -> "Invalid profile data"
                        _ -> "Invalid age format"
                _ -> "Invalid input format"

sampleData :: [String]
sampleData =
    [ "john_doe john@example.com 30"
    , "alice alice@test.org 25"
    , "bob123 bob@company.net 40"
    , "a a@b.c 200"
    , "test_user invalid-email 35"
    ]module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | length xs < n = []
    | otherwise = avg : movingAverage n (tail xs)
    where
        window = take n xs
        avg = sum window / fromIntegral n

-- Example usage with a helper function
demoMovingAverage :: IO ()
demoMovingAverage = do
    let dataSeries = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
    putStrLn $ "Original series: " ++ show dataSeries
    putStrLn $ "3-period moving average: " ++ show (movingAverage 3 dataSeries)
    putStrLn $ "5-period moving average: " ++ show (movingAverage 5 dataSeries)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (\x -> x * 2 + 1)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

main :: IO ()
main = do
    let numbers = [1..20]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed numbers: " ++ show (sumProcessed numbers)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)