module DataProcessor where

import Data.Char (isDigit, isAlpha, toUpper)

validateEmail :: String -> Bool
validateEmail email = '@' `elem` email && '.' `elem` afterAt
  where afterAt = dropWhile (/= '@') email

validatePhone :: String -> Bool
validatePhone = all isDigit

normalizeName :: String -> String
normalizeName = unwords . map capitalize . words
  where capitalize [] = []
        capitalize (x:xs) = toUpper x : map toLower xs

sanitizeInput :: String -> String
sanitizeInput = filter (\c -> isAlpha c || isDigit c || c `elem` " -_@.")

processUserData :: String -> String -> String -> Maybe (String, String, String)
processUserData name email phone
  | not (validateEmail email) = Nothing
  | not (validatePhone phone) = Nothing
  | otherwise = Just (normalizeName name, sanitizeInput email, phone)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers