
module DataProcessor where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)

-- | Validates if a string contains only digits
validateDigits :: String -> Bool
validateDigits = all isDigit

-- | Safely parses an integer, returns Nothing on failure
safeParseInt :: String -> Maybe Int
safeParseInt s
    | validateDigits s = Just (read s)
    | otherwise = Nothing

-- | Normalizes phone number by removing spaces and dashes
normalizePhone :: String -> String
normalizePhone = filter (\c -> isDigit c || c == '+')

-- | Validates email format (basic check)
validateEmail :: String -> Bool
validateEmail email =
    let parts = split '@' email
    in length parts == 2 &&
       not (null (head parts)) &&
       '.' `elem` (last parts)
  where
    split delimiter = foldr (\c acc ->
        if c == delimiter
        then []:acc
        else (c:head acc):tail acc) [[]]

-- | Transforms a list of strings to uppercase
toUpperAll :: [String] -> [String]
toUpperAll = map (map toUpper)
  where
    toUpper c
        | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
        | otherwise = c

-- | Combines first and last name with proper formatting
formatFullName :: String -> String -> String
formatFullName firstName lastName =
    intercalate " " [capitalize firstName, capitalize lastName]
  where
    capitalize [] = []
    capitalize (x:xs) = toUpper x : xs
    toUpper c
        | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
        | otherwise = c

-- | Processes a list of potential integers, returning valid ones
processNumbers :: [String] -> [Int]
processNumbers = catMaybes . map safeParseInt

-- | Trims whitespace from both ends of a string
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- | Main data processing pipeline example
processUserData :: [(String, String, String)] -> [(String, Int)]
processUserData users =
    map (\(name, phone, ageStr) ->
        (formatFullName name "User", fromMaybe 0 (safeParseInt ageStr))
    ) users