
module DataProcessor where

import Data.Char (isDigit, isAlpha)
import Data.List (intercalate)

type ValidationRule = String -> Bool
type Transformation = String -> String

validateNumeric :: ValidationRule
validateNumeric = all isDigit

validateAlpha :: ValidationRule
validateAlpha = all isAlpha

validateLength :: Int -> ValidationRule
validateLength n s = length s == n

transformToUpper :: Transformation
transformToUpper = map toUpper

transformPadLeft :: Int -> Char -> Transformation
transformPadLeft n c s = replicate (n - length s) c ++ s

processField :: ValidationRule -> Transformation -> String -> Maybe String
processField validate transform input =
    if validate input
        then Just $ transform input
        else Nothing

processCSVRow :: [String] -> [ValidationRule] -> [Transformation] -> Maybe [String]
processCSVRow row validations transforms =
    sequence $ zipWith3 processField validations transforms row

validateCSV :: [[String]] -> [ValidationRule] -> [Transformation] -> ([[String]], [String])
validateCSV rows validations transforms =
    foldr processRow ([], []) rows
  where
    processRow row (success, errors) =
        case processCSVRow row validations transforms of
            Just processed -> (processed:success, errors)
            Nothing -> (success, show row : errors)

formatErrors :: [String] -> String
formatErrors errors =
    "Validation failed for rows:\n" ++ intercalate "\n" errors

sampleData :: [[String]]
sampleData =
    [ ["123", "abc", "2023"]
    , ["456", "def", "2024"]
    , ["78x", "ghi", "2025"]
    ]

sampleValidations :: [ValidationRule]
sampleValidations =
    [ validateNumeric
    , validateAlpha
    , validateLength 4
    ]

sampleTransforms :: [Transformation]
sampleTransforms =
    [ transformPadLeft 5 '0'
    , transformToUpper
    , id
    ]module DataProcessor where

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

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

processOddCubes :: [Int] -> [Int]
processOddCubes = filterAndTransform odd (\x -> x * x * x)

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn "Original list:"
    print numbers
    putStrLn "\nEven numbers squared:"
    print $ processEvenSquares numbers
    putStrLn "\nOdd numbers cubed:"
    print $ processOddCubes numbers
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

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData = all (> 0)

main :: IO ()
main = do
    let sampleData = [1, -2, 3, -4, 5]
    putStrLn $ "Original data: " ++ show sampleData
    putStrLn $ "Processed data: " ++ show (processData sampleData)
    putStrLn $ "Is valid? " ++ show (validateData sampleData)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 10) (* 2)
module DataProcessor where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ",") (lines content)

safeReadDouble :: String -> Maybe Double
safeReadDouble s = case reads s of
    [(val, "")] -> Just val
    _ -> Nothing

calculateColumnAverage :: CSVData -> Int -> Maybe Double
calculateColumnAverage rows colIndex
    | null validValues = Nothing
    | otherwise = Just (sum validValues / fromIntegral (length validValues))
  where
    columnValues = mapMaybe (safeGetColumn colIndex) rows
    validValues = mapMaybe safeReadDouble columnValues
    safeGetColumn idx row
        | idx >= 0 && idx < length row = Just (row !! idx)
        | otherwise = Nothing

processCSVFile :: String -> IO (Maybe Double)
processCSVFile filename = do
    content <- readFile filename
    let parsed = parseCSV content
    return $ calculateColumnAverage parsed 2
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedList :: [Int] -> Int
sumProcessedList = sum . processEvenSquares

main :: IO ()
main = do
    let sampleData = [1..10]
    let result = sumProcessedList sampleData
    putStrLn $ "Sum of squares of even numbers from 1 to 10: " ++ show result
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
    putStrLn $ "Sum of processed data: " ++ show (sumProcessedData sampleData)module DataProcessor where

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

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Even squares: " ++ show (processEvenSquares numbers)
    putStrLn $ "First element: " ++ show (safeHead numbers)module DataProcessor where

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