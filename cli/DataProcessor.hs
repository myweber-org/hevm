
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 10) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, 0, 5, -7]
    let result = processData input
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers
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
    putStrLn $ "Sum of processed data: " ++ show (sumProcessedData sampleData)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

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
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-5, 3, 0, 8, -2, 10]
    let result = processNumbers numbers
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = all (\x -> x >= -100 && x <= 100) xs

safeProcess :: [Int] -> Maybe [Int]
safeProcess xs
  | validateInput xs = Just (processData xs)
  | otherwise = Nothingmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transform = map transform . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processEvenSquares

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processEvenSquares
module DataProcessor where

import Data.Char (isDigit, isAlpha)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)

-- Type for validated personal records
data PersonalRecord = PersonalRecord
    { prName :: String
    , prAge :: Int
    , prEmail :: String
    } deriving (Show, Eq)

-- Safe age parser with validation
parseAge :: String -> Maybe Int
parseAge str
    | all isDigit str && not (null str) = 
        let age = read str
        in if age >= 0 && age <= 150 then Just age else Nothing
    | otherwise = Nothing

-- Email validator (basic)
validateEmail :: String -> Bool
validateEmail email =
    let parts = splitOn '@' email
    in length parts == 2 && 
       not (null (head parts)) && 
       '.' `elem` (last parts) &&
       all validEmailChar email
  where
    validEmailChar c = isAlpha c || isDigit c || c `elem` "._%+-"
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr f [[]]
      where
        f c acc@(x:xs)
            | c == delimiter = []:acc
            | otherwise = (c:x):xs

-- Name sanitizer
sanitizeName :: String -> String
sanitizeName = unwords . filter (not . null) . map capitalize . words
  where
    capitalize [] = []
    capitalize (x:xs) = toUpper x : map toLower xs
    toUpper = toEnum . fromEnum . toUpperEnum
    toLower = toEnum . fromEnum . toLowerEnum
    toUpperEnum c
        | c >= 'a' && c <= 'z' = fromEnum c - 32
        | otherwise = fromEnum c
    toLowerEnum c
        | c >= 'A' && c <= 'Z' = fromEnum c + 32
        | otherwise = fromEnum c

-- Record constructor with validation
createRecord :: String -> String -> String -> Maybe PersonalRecord
createRecord name ageStr emailStr = do
    age <- parseAge ageStr
    guard $ validateEmail emailStr
    let cleanName = sanitizeName name
    guard $ not (null cleanName)
    return $ PersonalRecord cleanName age emailStr

-- Batch processor for CSV-like data
processRecords :: [String] -> [PersonalRecord]
processRecords = mapMaybe processLine
  where
    processLine line = case splitOn ',' line of
        [name, age, email] -> createRecord name age email
        _ -> Nothing

-- Utility to format records as CSV
formatAsCSV :: [PersonalRecord] -> String
formatAsCSV records =
    "Name,Age,Email\n" ++
    intercalate "\n" (map formatRecord records)
  where
    formatRecord (PersonalRecord name age email) =
        intercalate "," [name, show age, email]

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
    let rawData = 
            [ "john doe,30,john@example.com"
            , "jane smith,25,jane.smith@test.org"
            , "invalid data,abc,not-an-email"
            ]
    
    putStrLn "Processing records..."
    let records = processRecords rawData
    
    putStrLn $ "Valid records found: " ++ show (length records)
    putStrLn "\nFormatted output:"
    putStrLn $ formatAsCSV records