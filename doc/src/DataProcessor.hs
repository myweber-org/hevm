module DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter (>0)

validateData :: [Int] -> Bool
validateData xs = all (>=0) xs && not (null xs)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    if validateData input
        then print $ processData input
        else putStrLn "Invalid input data"module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (>= -100) xs then Just xs else Nothing

main :: IO ()
main = do
    let sampleData = [1, -5, 3, 0, 8, -2]
    case validateInput sampleData of
        Just validData -> do
            let result = sumProcessedData validData
            putStrLn $ "Sum of processed data: " ++ show result
        Nothing -> putStrLn "Input contains values less than -100"module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processData

validateInput :: [Int] -> Maybe [Int]
validateInput xs
    | null xs = Nothing
    | otherwise = Just xs

main :: IO ()
main = do
    let sampleData = [-3, 1, 0, 5, -2, 8]
    case validateInput sampleData of
        Nothing -> putStrLn "Empty input list"
        Just data' -> do
            putStrLn $ "Original data: " ++ show data'
            putStrLn $ "Processed data: " ++ show (processData data')
            putStrLn $ "Sum of positive doubles: " ++ show (sumPositiveDoubles data')module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isAlpha)

data ValidationError = InvalidFormat String | MissingField String | InvalidValue String
    deriving (Show, Eq)

type CSVRow = [String]
type ValidatedRow = Either ValidationError CSVRow

validateRow :: CSVRow -> ValidatedRow
validateRow row
    | length row < 3 = Left $ MissingField "Row must contain at least 3 fields"
    | not (all validField row) = Left $ InvalidFormat "Fields contain invalid characters"
    | not (validId (head row)) = Left $ InvalidValue "First field must be numeric ID"
    | not (validName (row !! 1)) = Left $ InvalidValue "Second field must be alphabetic name"
    | otherwise = Right row
  where
    validField = not . any (`elem` ",;\"\n\r\t")
    validId = all isDigit
    validName = all isAlpha

processCSVData :: [CSVRow] -> ([CSVRow], [ValidationError])
processCSVData rows = foldr processRow ([], []) rows
  where
    processRow row (validRows, errors) =
        case validateRow row of
            Right valid -> (valid:validRows, errors)
            Left err -> (validRows, err:errors)

formatErrors :: [ValidationError] -> String
formatErrors errors = intercalate "\n" $ map formatError errors
  where
    formatError (InvalidFormat msg) = "Format error: " ++ msg
    formatError (MissingField msg) = "Missing field: " ++ msg
    formatError (InvalidValue msg) = "Invalid value: " ++ msg

sampleData :: [CSVRow]
sampleData =
    [ ["123", "John", "Data1", "Extra"]
    , ["456", "Alice", "Data2"]
    , ["789", "Bob123", "Data3"]
    , ["abc", "Charlie", "Data4"]
    , ["999", "David", "Data,5"]
    ]

main :: IO ()
main = do
    let (validRows, errors) = processCSVData sampleData
    putStrLn "Valid rows:"
    mapM_ print validRows
    putStrLn "\nValidation errors:"
    putStrLn $ formatErrors errors