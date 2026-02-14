module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processData
module DataProcessor where

import Data.Char (isDigit, isAlpha, toUpper)

-- Validate if a string contains only digits
validateNumeric :: String -> Bool
validateNumeric = all isDigit

-- Validate if a string contains only alphabetic characters
validateAlpha :: String -> Bool
validateAlpha = all isAlpha

-- Transform string to uppercase
transformToUpper :: String -> String
transformToUpper = map toUpper

-- Process a list of strings with validation and transformation
processData :: [String] -> [(String, Bool, String)]
processData = map processSingle
  where
    processSingle str =
      let isNum = validateNumeric str
          isAlphaStr = validateAlpha str
          transformed = transformToUpper str
      in (str, isNum && not isAlphaStr, transformed)

-- Filter numeric strings from processed data
extractNumeric :: [(String, Bool, String)] -> [String]
extractNumeric = map (\(orig, _, _) -> orig) . filter (\(_, isNum, _) -> isNum)

-- Main processing pipeline
pipeline :: [String] -> [String]
pipeline = extractNumeric . processData