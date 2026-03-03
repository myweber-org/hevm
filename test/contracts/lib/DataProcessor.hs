
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 10) (* 2)

validateData :: [Int] -> Bool
validateData xs = all (\x -> x `mod` 2 == 0) (processData xs)

main :: IO ()
main = do
    let sampleData = [5, 12, 8, 20, 3, 15]
    putStrLn "Original data:"
    print sampleData
    putStrLn "Processed data:"
    print (processData sampleData)
    putStrLn "Validation result:"
    print (validateData sampleData)
module DataProcessor where

import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)
import Text.Read (readMaybe)

-- Safe integer parsing with default value
safeReadInt :: String -> Int -> Int
safeReadInt str def = fromMaybe def (readMaybe str)

-- Validate email format (basic check)
isValidEmail :: String -> Bool
isValidEmail email =
    let parts = split '@' email
    in length parts == 2 
       && not (null (head parts)) 
       && '.' `elem` (last parts)
  where
    split delimiter = foldr (\c acc -> if c == delimiter then []:acc else (c:head acc):tail acc) [[]]

-- Clean phone number (keep only digits)
cleanPhone :: String -> String
cleanPhone = filter isDigit

-- Transform name to proper case
toProperCase :: String -> String
toProperCase = unwords . map capitalize . words
  where
    capitalize [] = []
    capitalize (x:xs) = toUpper x : map toLower xs
    toUpper = toEnum . subtract 32 . fromEnum
    toLower c = if c >= 'A' && c <= 'Z' 
                then toEnum (fromEnum c + 32) 
                else c

-- Parse CSV line with validation
parseCSVLine :: String -> Maybe [String]
parseCSVLine line
    | null line = Nothing
    | otherwise = Just $ parseFields line False ""
  where
    parseFields "" _ _ = []
    parseFields (c:cs) inQuotes current
        | c == '"' = parseFields cs (not inQuotes) current
        | c == ',' && not inQuotes = current : parseFields cs False ""
        | otherwise = parseFields cs inQuotes (current ++ [c])

-- Data validation result type
data Validation a = Valid a | Invalid String

-- Validate user age
validateAge :: Int -> Validation Int
validateAge age
    | age < 0 = Invalid "Age cannot be negative"
    | age > 150 = Invalid "Age seems unrealistic"
    | otherwise = Valid age

-- Process user data record
processUserRecord :: [String] -> [String]
processUserRecord [name, email, phone, ageStr] =
    let cleanName = toProperCase name
        cleanEmail = if isValidEmail email then email else "invalid@example.com"
        cleanPhoneNum = cleanPhone phone
        age = safeReadInt ageStr 0
        validatedAge = case validateAge age of
                        Valid a -> show a
                        Invalid err -> "0 (" ++ err ++ ")"
    in [cleanName, cleanEmail, cleanPhoneNum, validatedAge]
processUserRecord _ = ["Invalid", "record", "format", "0"]

-- Main processing pipeline
processData :: [String] -> [[String]]
processData = map processUserRecord . catMaybes . map parseCSVLine

-- Format output as CSV
formatOutput :: [[String]] -> String
formatOutput = intercalate "\n" . map (intercalate ",")module DataProcessor where

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
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 10) (* 2)