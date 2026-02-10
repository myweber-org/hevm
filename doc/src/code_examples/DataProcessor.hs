
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
    ) usersmodule DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> Either String CSVData
parseCSV content = 
    if null content
    then Left "Empty CSV content"
    else Right $ map (splitOnComma . escapeCommas) (lines content)
  where
    splitOnComma :: String -> Row
    splitOnComma [] = []
    splitOnComma str = 
        let (cell, rest) = extractCell str
        in cell : splitOnComma (dropWhile (== ',') rest)
    
    extractCell :: String -> (String, String)
    extractCell ('"' : rest) = extractQuotedCell rest ""
    extractCell str = let (cell, rest) = span (/= ',') str
                      in (cell, rest)
    
    extractQuotedCell :: String -> String -> (String, String)
    extractQuotedCell [] acc = (reverse acc, "")
    extractQuotedCell ('"' : '"' : rest) acc = extractQuotedCell rest ('"' : acc)
    extractQuotedCell ('"' : rest) acc = (reverse acc, rest)
    extractQuotedCell (c : rest) acc = extractQuotedCell rest (c : acc)
    
    escapeCommas :: String -> String
    escapeCommas [] = []
    escapeCommas ('"' : rest) = '"' : processQuoted rest
    escapeCommas (c : rest) = c : escapeCommas rest
    
    processQuoted :: String -> String
    processQuoted [] = []
    processQuoted ('"' : '"' : rest) = '"' : '"' : processQuoted rest
    processQuoted ('"' : rest) = '"' : escapeCommas rest
    processQuoted (c : rest) = c : processQuoted rest

validateNumericColumn :: CSVData -> Int -> Either String CSVData
validateNumericColumn [] _ = Right []
validateNumericColumn rows colIndex
    | colIndex < 0 = Left "Column index must be non-negative"
    | otherwise = 
        let validated = map (validateRow colIndex) rows
        in if all isValid validated
           then Right rows
           else Left $ "Invalid numeric data in column " ++ show colIndex
  where
    validateRow :: Int -> Row -> Bool
    validateRow idx row
        | idx >= length row = False
        | otherwise = all isDigit (filter (/= '.') (row !! idx))
    
    isValid :: Bool -> Bool
    isValid = id

calculateColumnAverage :: CSVData -> Int -> Either String Double
calculateColumnAverage rows colIndex
    | colIndex < 0 = Left "Column index must be non-negative"
    | null rows = Left "No data to process"
    | otherwise = 
        let numericValues = mapMaybe (getNumericValue colIndex) rows
        in if null numericValues
           then Left "No valid numeric values found"
           else Right (sum numericValues / fromIntegral (length numericValues))
  where
    getNumericValue :: Int -> Row -> Maybe Double
    getNumericValue idx row
        | idx >= length row = Nothing
        | otherwise = case reads (row !! idx) of
                        [(val, "")] -> Just val
                        _ -> Nothing

formatCSVOutput :: CSVData -> String
formatCSVOutput = intercalate "\n" . map (intercalate "," . map escapeCell)
  where
    escapeCell :: String -> String
    escapeCell cell
        | any (\c -> c == ',' || c == '"' || c == '\n') cell = "\"" ++ concatMap escapeChar cell ++ "\""
        | otherwise = cell
    
    escapeChar :: Char -> String
    escapeChar '"' = "\"\""
    escapeChar c = [c]

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = case f x of
    Just y  -> y : mapMaybe f xs
    Nothing -> mapMaybe f xsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-5, 3, 0, 8, -2, 10]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (^2)

calculateStats :: [Int] -> (Int, Int, Double)
calculateStats [] = (0, 0, 0.0)
calculateStats xs = 
    let total = sum xs
        count = length xs
        average = fromIntegral total / fromIntegral count
    in (total, count, average)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x