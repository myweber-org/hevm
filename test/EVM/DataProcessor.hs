
module DataProcessor where

import Data.Time
import Data.Time.Format
import Data.List
import Data.Char

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

type Record = (Day, String, Double)

parseRecord :: String -> Maybe Record
parseRecord line = case words line of
    [dateStr, desc, amountStr] -> do
        date <- parseDate dateStr
        amount <- readMaybe amountStr
        return (date, desc, amount)
    _ -> Nothing
  where
    readMaybe :: String -> Maybe Double
    readMaybe s = case reads s of
        [(x, "")] -> Just x
        _ -> Nothing

filterByDateRange :: Day -> Day -> [Record] -> [Record]
filterByDateRange start end = filter (\(date, _, _) -> date >= start && date <= end)

summarizeRecords :: [Record] -> [(String, Double)]
summarizeRecords = map summarize . groupBy sameDesc . sortBy compareDesc
  where
    sameDesc (_, desc1, _) (_, desc2, _) = desc1 == desc2
    compareDesc (_, desc1, _) (_, desc2, _) = compare desc1 desc2
    summarize group@((_, desc, _):_) = (desc, sum $ map (\(_, _, a) -> a) group)

processData :: String -> Day -> Day -> Either String [(String, Double)]
processData input start end = do
    let records = mapMaybe parseRecord (lines input)
    if null records
        then Left "No valid records found"
        else Right $ summarizeRecords $ filterByDateRange start end recordsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

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
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = all (\x -> x >= -100 && x <= 100) xs

safeProcess :: [Int] -> Maybe [Int]
safeProcess xs
  | validateInput xs = Just $ processData xs
  | otherwise = Nothing

exampleUsage :: IO ()
exampleUsage = do
  let input = [-5, 2, 0, 8, -3, 10]
  case safeProcess input of
    Just result -> putStrLn $ "Processed result: " ++ show result
    Nothing -> putStrLn "Invalid input detected"
module DataProcessor where

import Data.Char (isDigit, toUpper)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

-- Safe string to integer conversion
safeReadInt :: String -> Maybe Int
safeReadInt str
    | all isDigit str = Just (read str)
    | otherwise = Nothing

-- Normalize string by trimming and converting to uppercase
normalizeString :: String -> String
normalizeString = map toUpper . trim
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- Validate email format (basic validation)
validateEmail :: String -> Bool
validateEmail email =
    let (local, rest) = break (== '@') email
        domain = drop 1 rest
    in '@' `elem` email &&
       not (null local) &&
       '.' `elem` domain &&
       not (null (takeWhile (/= '.') domain))

-- Process a list of strings into a formatted CSV line
formatAsCSV :: [String] -> String
formatAsCSV = intercalate "," . map escapeCSV
  where
    escapeCSV s
        | ',' `elem` s || '"' `elem` s = "\"" ++ replace '"' "\"\"" s ++ "\""
        | otherwise = s
    replace old new = intercalate new . splitOn old
    splitOn delim = foldr splitHelper [[]]
      where
        splitHelper c acc@(x:xs)
            | c == delim = []:acc
            | otherwise = (c:x):xs

-- Type-safe data transformation pipeline
data ProcessResult a = Success a | ValidationError String | ProcessingError String

instance Functor ProcessResult where
    fmap f (Success x) = Success (f x)
    fmap _ (ValidationError e) = ValidationError e
    fmap _ (ProcessingError e) = ProcessingError e

processUserData :: String -> String -> ProcessResult (Int, String)
processUserData idStr nameStr = do
    userId <- case safeReadInt idStr of
        Just n -> Success n
        Nothing -> ValidationError "Invalid user ID format"
    
    let normalizedName = normalizeString nameStr
    if null normalizedName
        then ValidationError "Name cannot be empty"
        else Success (userId, normalizedName)

-- Utility function to demonstrate usage
demoProcessing :: IO ()
demoProcessing = do
    let testCases = [("123", "john doe"), ("abc", "jane smith"), ("456", "")]
    
    mapM_ (\(idStr, name) -> 
        case processUserData idStr name of
            Success (uid, normName) -> 
                putStrLn $ "Valid: User " ++ show uid ++ " -> " ++ normName
            ValidationError err -> 
                putStrLn $ "Invalid: " ++ err
            ProcessingError err -> 
                putStrLn $ "Error: " ++ err
        ) testCases
module DataProcessor where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import Data.Maybe (catMaybes, listToMaybe)
import Text.Read (readMaybe)

-- | Validates if a string contains only digits
validateDigits :: String -> Bool
validateDigits = all isDigit

-- | Parses a string into an integer safely
safeParseInt :: String -> Maybe Int
safeParseInt s = 
    if validateDigits s then readMaybe s else Nothing

-- | Trims leading and trailing whitespace
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

-- | Normalizes a string by trimming and converting to lowercase
normalizeString :: String -> String
normalizeString = map toLower . trim
    where toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

-- | Validates email format (simple version)
validateEmail :: String -> Bool
validateEmail email =
    let trimmed = trim email
        parts = splitOn '@' trimmed
    in length parts == 2 
       && not (null (head parts)) 
       && '.' `elem` (last parts)
    where splitOn :: Char -> String -> [String]
          splitOn delimiter = foldr (\c acc -> 
              if c == delimiter 
              then []:acc 
              else (c:head acc):tail acc) [[]]

-- | Transforms a list of strings into a CSV row
toCSVRow :: [String] -> String
toCSVRow = intercalate "," . map escapeCSV
    where escapeCSV s = if ',' `elem` s || '"' `elem` s 
                        then '"' : concatMap escapeChar s ++ "\""
                        else s
          escapeChar '"' = "\"\""
          escapeChar c   = [c]

-- | Processes a list of optional values, keeping only successful ones
processValidData :: [Maybe a] -> [a]
processValidData = catMaybes

-- | Extracts the first valid value from a list of maybes
firstValid :: [Maybe a] -> Maybe a
firstValid = listToMaybe . catMaybes

-- | Validates and transforms user input data
processUserInput :: String -> Either String (Int, String)
processUserInput input =
    case words input of
        [numStr, nameStr] ->
            case safeParseInt numStr of
                Just num -> 
                    let normalizedName = normalizeString nameStr
                    in if length normalizedName >= 2
                       then Right (num, normalizedName)
                       else Left "Name must be at least 2 characters"
                Nothing -> Left "Invalid number format"
        _ -> Left "Expected format: <number> <name>"
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print result