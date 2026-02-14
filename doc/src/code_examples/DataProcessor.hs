
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
safeHead (x:_) = Just xmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

import Data.Char (isDigit, isAlpha)
import Data.List (intercalate)

type ValidationRule = String -> Bool
type Transformation = String -> String

validateNumeric :: ValidationRule
validateNumeric = all isDigit

validateAlpha :: ValidationRule
validateAlpha = all isAlpha

transformToUpper :: Transformation
transformToUpper = map toUpper

transformToLower :: Transformation
transformToLower = map toLower

sanitizePhone :: Transformation
sanitizePhone = filter isDigit

processCSVRow :: [ValidationRule] -> [Transformation] -> [String] -> Either String [String]
processCSVRow validators transformers row
    | length row /= length validators = Left "Row length doesn't match validator count"
    | not (all id $ zipWith ($) validators row) = Left "Validation failed"
    | otherwise = Right $ zipWith ($) transformers row

formatCSV :: [String] -> String
formatCSV = intercalate ","

validateAndTransform :: [[String]] -> Either String [[String]]
validateAndTransform rows = 
    let phoneValidator = validateNumeric
        nameValidator = validateAlpha
        validators = [phoneValidator, nameValidator]
        
        phoneTransformer = sanitizePhone
        nameTransformer = transformToUpper
        transformers = [phoneTransformer, nameTransformer]
    in sequence $ map (processCSVRow validators transformers) rowsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData xs = all (> 0) xs && length xs > 3

combineProcessors :: [Int] -> [Int]
combineProcessors xs = if validateData xs then processData xs else []module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 10) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateData :: [Int] -> Bool
validateData xs = all (> 0) xs && length xs > 0

main :: IO ()
main = do
    let testData = [5, 12, 8, 15, 3, 20]
    if validateData testData
        then do
            putStrLn "Original data:"
            print testData
            putStrLn "Processed data:"
            print $ processData testData
            putStrLn "Sum of processed data:"
            print $ sumProcessedData testData
        else putStrLn "Invalid input data"module DataProcessor where

import Data.List (intercalate)
import Data.Char (toLower)

type CSVRow = [String]

parseCSV :: String -> [CSVRow]
parseCSV content = map (splitOn ',') (lines content)
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitHelper [""]
      where
        splitHelper :: Char -> [String] -> [String]
        splitHelper ch (x:xs)
          | ch == delimiter = "":x:xs
          | otherwise = (ch:x):xs

filterRows :: (CSVRow -> Bool) -> [CSVRow] -> [CSVRow]
filterRows predicate = filter predicate

containsKeyword :: String -> CSVRow -> Bool
containsKeyword keyword row = any (keyword `isInfixOf`) (map toLower <$> row)

formatCSV :: [CSVRow] -> String
formatCSV rows = intercalate "\n" (map (intercalate ",") rows)

processCSVData :: String -> String -> String
processCSVData keyword csvContent =
  let parsed = parseCSV csvContent
      filtered = filterRows (containsKeyword keyword) parsed
  in formatCSV filteredmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

main :: IO ()
main = do
    let numbers = [-3, 1, 4, -2, 5, 0]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)
    putStrLn $ "First element: " ++ show (safeHead numbers)module DataProcessor where

import Data.Char (toLower, isAlpha, isSpace)
import Data.List (intercalate)

data UserProfile = UserProfile
    { userName :: String
    , userEmail :: String
    , userAge :: Int
    } deriving (Show, Eq)

validateName :: String -> Maybe String
validateName name
    | all (\c -> isAlpha c || isSpace c) name && not (null name) = Just name
    | otherwise = Nothing

normalizeEmail :: String -> String
normalizeEmail = map toLower . filter (/= ' ')

validateAge :: Int -> Maybe Int
validateAge age
    | age >= 0 && age <= 150 = Just age
    | otherwise = Nothing

createProfile :: String -> String -> Int -> Maybe UserProfile
createProfile name email age = do
    validName <- validateName name
    validAge <- validateAge age
    let normalizedEmail = normalizeEmail email
    return $ UserProfile validName normalizedEmail validAge

profileToString :: UserProfile -> String
profileToString profile =
    intercalate ", " 
        [ "Name: " ++ userName profile
        , "Email: " ++ userEmail profile
        , "Age: " ++ show (userAge profile)
        ]

processProfiles :: [UserProfile] -> [String]
processProfiles = map profileToString . filter (\p -> userAge p >= 18)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -100) xs then Just xs else Nothing

main :: IO ()
main = do
    let sampleData = [1, -5, 3, 0, 8, -2]
    case validateInput sampleData of
        Just validData -> do
            putStrLn $ "Original data: " ++ show validData
            putStrLn $ "Processed data: " ++ show (processData validData)
            putStrLn $ "Sum of processed data: " ++ show (sumProcessedData validData)
        Nothing -> putStrLn "Input validation failed: values must be greater than -100"module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    print result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)
module DataProcessor where

import Data.List (sort, nub)

-- Filter even numbers from a list
filterEvens :: [Int] -> [Int]
filterEvens = filter even

-- Square each element in a list
squareAll :: [Int] -> [Int]
squareAll = map (^2)

-- Remove duplicates and sort a list
uniqueSorted :: [Int] -> [Int]
uniqueSorted = sort . nub

-- Combine filtering and transformation
processNumbers :: [Int] -> [Int]
processNumbers = uniqueSorted . squareAll . filterEvens

-- Calculate sum of processed numbers
sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
    let numbers = [1,2,3,4,5,6,7,8,9,10,2,4,6,8,10]
    putStrLn $ "Original: " ++ show numbers
    putStrLn $ "Filtered evens: " ++ show (filterEvens numbers)
    putStrLn $ "Squared: " ++ show (squareAll numbers)
    putStrLn $ "Processed: " ++ show (processNumbers numbers)
    putStrLn $ "Sum processed: " ++ show (sumProcessed numbers)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Maybe [Int]
validateInput xs
    | null xs = Nothing
    | any (< -100) xs = Nothing
    | any (> 100) xs = Nothing
    | otherwise = Just xs