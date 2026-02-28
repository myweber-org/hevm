module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData xs = all (> 0) xs && length xs > 3

combineProcessors :: [Int] -> [Int]
combineProcessors xs = if validateData xs then processData xs else []
module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input = 
    if null input
    then Left "Empty input"
    else Right $ map parseRow (lines input)
  where
    parseRow line = splitByComma line
    splitByComma = foldr (\c acc -> if c == ',' then []:acc else (c:head acc):tail acc) [[]]

validateNumericColumn :: CSVData -> Int -> Either String CSVData
validateNumericColumn [] _ = Left "Empty data"
validateNumericColumn rows colIndex
    | colIndex < 0 = Left "Column index cannot be negative"
    | any (\row -> length row <= colIndex) rows = Left "Column index out of bounds"
    | not (all (isNumeric . (!! colIndex)) rows) = Left $ "Column " ++ show colIndex ++ " contains non-numeric values"
    | otherwise = Right rows
  where
    isNumeric = all isDigit

calculateColumnAverage :: CSVData -> Int -> Either String Double
calculateColumnAverage rows colIndex = do
    validated <- validateNumericColumn rows colIndex
    let values = map (read . (!! colIndex)) validated
    return $ sum values / fromIntegral (length values)

formatCSVOutput :: CSVData -> String
formatCSVOutput = intercalate "\n" . map (intercalate ",")

processCSVFile :: String -> Int -> Either String (String, Double)
processCSVFile content colIndex = do
    parsed <- parseCSV content
    avg <- calculateColumnAverage parsed colIndex
    return (formatCSVOutput parsed, avg)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform even (* 2)
module DataProcessor where

import Data.Char (toUpper, isDigit)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)

-- Type for validated person data
data Person = Person
    { firstName :: String
    , lastName  :: String
    , age       :: Int
    , email     :: String
    } deriving (Show, Eq)

-- Validate and create a Person
validatePerson :: String -> String -> Int -> String -> Maybe Person
validatePerson first last a emailAddr
    | null first || null last = Nothing
    | a < 0 || a > 150 = Nothing
    | not (isValidEmail emailAddr) = Nothing
    | otherwise = Just $ Person (capitalize first) (capitalize last) a emailAddr

-- Capitalize first letter of a string
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

-- Simple email validation
isValidEmail :: String -> Bool
isValidEmail email =
    let parts = split '@' email
    in length parts == 2 && 
       not (null (head parts)) && 
       '.' `elem` (last parts)
  where
    split delimiter = foldr (\c acc -> if c == delimiter then []:acc else (c:head acc):tail acc) [[]]

-- Process a list of raw data into validated persons
processPersonData :: [(String, String, Int, String)] -> [Person]
processPersonData = mapMaybe (\(f, l, a, e) -> validatePerson f l a e)

-- Format person data for display
formatPerson :: Person -> String
formatPerson p = 
    intercalate " | " 
        [ lastName p ++ ", " ++ firstName p
        , show (age p)
        , email p
        ]

-- Calculate average age
averageAge :: [Person] -> Double
averageAge [] = 0.0
averageAge persons = 
    fromIntegral (sum (map age persons)) / fromIntegral (length persons)

-- Filter persons by age range
filterByAgeRange :: Int -> Int -> [Person] -> [Person]
filterByAgeRange minAge maxAge = 
    filter (\p -> age p >= minAge && age p <= maxAge)

-- Example usage
exampleData :: [(String, String, Int, String)]
exampleData =
    [ ("john", "doe", 30, "john@example.com")
    , ("jane", "smith", 25, "jane.smith@test.org")
    , ("bob", "brown", 45, "invalid-email")
    , ("alice", "jones", 19, "alice@company.co")
    ]

processExample :: IO ()
processExample = do
    let persons = processPersonData exampleData
    putStrLn "Validated Persons:"
    mapM_ (putStrLn . formatPerson) persons
    putStrLn $ "Average age: " ++ show (averageAge persons)
    putStrLn $ "Persons aged 20-40: " ++ show (length (filterByAgeRange 20 40 persons))module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Maybe [Int]
validateInput xs
    | null xs = Nothing
    | any (< -1000) xs = Nothing
    | any (> 1000) xs = Nothing
    | otherwise = Just xsmodule DataProcessor where

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
        Nothing -> putStrLn "Invalid input: contains numbers less than -100"