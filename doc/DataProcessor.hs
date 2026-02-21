module DataProcessor where

import Data.Char (isDigit, isAlpha, toLower)
import Data.List (intercalate)

-- Validate that a string contains only alphabetic characters
validateAlpha :: String -> Bool
validateAlpha = all isAlpha

-- Validate that a string contains only numeric characters
validateNumeric :: String -> Bool
validateNumeric = all isDigit

-- Normalize a string by converting to lowercase and trimming whitespace
normalizeString :: String -> String
normalizeString = unwords . words . map toLower

-- Transform a list of strings into a comma-separated string
joinWithCommas :: [String] -> String
joinWithCommas = intercalate ", "

-- Validate email format (basic validation)
validateEmail :: String -> Bool
validateEmail email =
    let parts = splitAt (length email - 1) email
    in '@' `elem` fst parts && '.' `elem` snd parts

-- Split a string into words and capitalize each word
capitalizeWords :: String -> String
capitalizeWords = unwords . map capitalizeWord . words
  where
    capitalizeWord [] = []
    capitalizeWord (x:xs) = toUpper x : xs
    toUpper c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c

-- Filter out empty strings from a list
filterNonEmpty :: [String] -> [String]
filterNonEmpty = filter (not . null)

-- Count the number of vowels in a string
countVowels :: String -> Int
countVowels = length . filter (`elem` "aeiouAEIOU")