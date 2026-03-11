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
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processDatamodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

import Data.List.Split (splitOn)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ",") (lines content)

extractNumericColumn :: CSVData -> Int -> Maybe [Double]
extractNumericColumn dataRows colIndex
    | null dataRows = Nothing
    | otherwise = sequence $ map (safeRead . (!! colIndex)) dataRows
  where
    safeRead s = case reads s of
        [(val, "")] -> Just val
        _ -> Nothing

calculateAverage :: [Double] -> Maybe Double
calculateAverage vals
    | null vals = Nothing
    | otherwise = Just (sum vals / fromIntegral (length vals))

processCSVFile :: String -> Int -> IO (Maybe Double)
processCSVFile filePath columnIndex = do
    content <- readFile filePath
    let parsedData = parseCSV content
    case extractNumericColumn parsedData columnIndex of
        Nothing -> return Nothing
        Just numbers -> return $ calculateAverage numbers

main :: IO ()
main = do
    result <- processCSVFile "data.csv" 2
    case result of
        Nothing -> putStrLn "Failed to process CSV data"
        Just avg -> putStrLn $ "Average: " ++ show avgmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -100) xs then Just xs else Nothingmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print result
module DataProcessor where

import Data.Char (toLower, isAlpha, isSpace)
import Data.List (intercalate)
import Data.Maybe (catMaybes)

type Username = String
type Email = String
type Age = Int

data UserProfile = UserProfile
  { username :: Username
  , email :: Email
  , age :: Age
  } deriving (Show, Eq)

validateUsername :: Username -> Maybe Username
validateUsername name
  | length name < 3 = Nothing
  | length name > 20 = Nothing
  | not (all isValidUsernameChar name) = Nothing
  | otherwise = Just (normalizeUsername name)
  where
    isValidUsernameChar c = isAlpha c || c == '_' || c == '-'
    normalizeUsername = map toLower

validateEmail :: Email -> Maybe Email
validateEmail emailStr
  | '@' `notElem` emailStr = Nothing
  | '.' `notElem` (dropWhile (/= '@') emailStr) = Nothing
  | any isSpace emailStr = Nothing
  | otherwise = Just (map toLower emailStr)

validateAge :: Age -> Maybe Age
validateAge a
  | a < 0 = Nothing
  | a > 150 = Nothing
  | otherwise = Just a

createUserProfile :: Username -> Email -> Age -> Maybe UserProfile
createUserProfile un em ag = do
  validUsername <- validateUsername un
  validEmail <- validateEmail em
  validAge <- validateAge ag
  return $ UserProfile validUsername validEmail validAge

sanitizeInput :: String -> String
sanitizeInput = unwords . words . filter (/= '\0')

formatUserDisplay :: UserProfile -> String
formatUserDisplay user =
  intercalate " | "
    [ "User: " ++ username user
    , "Email: " ++ email user
    , "Age: " ++ show (age user)
    ]

processUserBatch :: [(Username, Email, Age)] -> [UserProfile]
processUserBatch = catMaybes . map (\(u,e,a) -> createUserProfile u e a)

calculateAverageAge :: [UserProfile] -> Double
calculateAverageAge users =
  if null users
    then 0.0
    else fromIntegral (sum (map age users)) / fromIntegral (length users)module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isSpace)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input
    | null (trim input) = Right []
    | otherwise = traverse parseRow (lines input)
  where
    parseRow line = case parseFields line [] "" False of
        Left err -> Left $ "Row parse error: " ++ err
        Right fields -> Right fields

    parseFields :: String -> [String] -> String -> Bool -> Either String [String]
    parseFields [] acc current inQuotes
        | inQuotes = Left "Unclosed quote"
        | otherwise = Right $ reverse (if null current then acc else current:acc)
    parseFields (c:cs) acc current inQuotes
        | c == '"' && not inQuotes = parseFields cs acc current True
        | c == '"' && inQuotes = 
            case cs of
                '"':rest -> parseFields rest acc (current ++ [c]) True
                ',':rest -> parseFields rest (current:acc) "" False
                [] -> Right $ reverse (current:acc)
                _ -> Left "Invalid quote escape"
        | c == ',' && not inQuotes = parseFields cs (current:acc) "" False
        | otherwise = parseFields cs acc (current ++ [c]) inQuotes

validateCSV :: CSVData -> Either String CSVData
validateCSV [] = Right []
validateCSV (header:rows) 
    | null header = Left "Empty header row"
    | any null header = Left "Header contains empty fields"
    | otherwise = do
        validatedRows <- traverse validateRow rows
        return (header:validatedRows)
  where
    validateRow row
        | length row /= length header = Left $ "Row length mismatch. Expected " ++ show (length header) ++ " got " ++ show (length row)
        | any null row = Left "Row contains empty fields"
        | otherwise = Right row

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

processCSV :: String -> Either String CSVData
processCSV input = parseCSV input >>= validateCSV

formatCSV :: CSVData -> String
formatCSV = intercalate "\n" . map (intercalate "," . map escapeField)
  where
    escapeField field
        | any (\c -> c == ',' || c == '"' || c == '\n') field = "\"" ++ concatMap escapeChar field ++ "\""
        | otherwise = field
    escapeChar '"' = "\"\""
    escapeChar c = [c]

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
    let csvContent = "Name,Age,City\nJohn,25,\"New York\"\nAlice,30,London"
    case processCSV csvContent of
        Left err -> putStrLn $ "Error: " ++ err
        Right data -> do
            putStrLn "Parsed CSV data:"
            mapM_ print data
            putStrLn "\nFormatted output:"
            putStrLn $ formatCSV datamodule DataProcessor where

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
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, 0, 5, -7]
    let result = processNumbers input
    print result