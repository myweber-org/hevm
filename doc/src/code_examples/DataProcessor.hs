module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenNumbers :: [Int] -> [Int]
processEvenNumbers = filterAndTransform even (* 2)

processOddNumbers :: [Int] -> [Int]
processOddNumbers = filterAndTransform odd (+ 1)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

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

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenNumbers :: [Int] -> [Int]
processEvenNumbers = filterAndTransform even (* 2)

processOddNumbers :: [Int] -> [Int]
processOddNumbers = filterAndTransform odd (+ 1)

sumProcessedData :: (Int -> Bool) -> (Int -> Int) -> [Int] -> Int
sumProcessedData predicate transformer = 
    sum . filterAndTransform predicate transformer

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original: " ++ show numbers
    putStrLn $ "Even doubled: " ++ show (processEvenNumbers numbers)
    putStrLn $ "Odd incremented: " ++ show (processOddNumbers numbers)
    putStrLn $ "Sum of even doubled: " ++ show (sumProcessedData even (* 2) numbers)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transform = map transform . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (* 2)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show result
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

data ValidationError
    = InvalidUsername String
    | InvalidEmail String
    | InvalidAge String
    deriving (Show, Eq)

validateUsername :: Username -> Either ValidationError Username
validateUsername name
    | null name = Left $ InvalidUsername "Username cannot be empty"
    | length name < 3 = Left $ InvalidUsername "Username must be at least 3 characters"
    | length name > 20 = Left $ InvalidUsername "Username cannot exceed 20 characters"
    | not (all isAlpha name) = Left $ InvalidUsername "Username must contain only letters"
    | otherwise = Right (map toLower name)

validateEmail :: Email -> Either ValidationError Email
validateEmail emailStr
    | '@' `notElem` emailStr = Left $ InvalidEmail "Email must contain @ symbol"
    | '.' `notElem` (dropWhile (/= '@') emailStr) = Left $ InvalidEmail "Email must contain domain with dot"
    | any isSpace emailStr = Left $ InvalidEmail "Email cannot contain spaces"
    | otherwise = Right (map toLower emailStr)

validateAge :: Age -> Either ValidationError Age
validateAge a
    | a < 0 = Left $ InvalidAge "Age cannot be negative"
    | a > 150 = Left $ InvalidAge "Age must be realistic (≤ 150)"
    | otherwise = Right a

createUserProfile :: Username -> Email -> Age -> Either [ValidationError] UserProfile
createUserProfile un em ag = case results of
    [] -> Right $ UserProfile validUsername validEmail validAge
    errs -> Left errs
  where
    usernameResult = validateUsername un
    emailResult = validateEmail em
    ageResult = validateAge ag
    
    validUsername = either (const "") id usernameResult
    validEmail = either (const "") id emailResult
    validAge = either (const 0) id ageResult
    
    results = catMaybes
        [ either (Just . InvalidUsername) (const Nothing) usernameResult
        , either (Just . InvalidEmail) (const Nothing) emailResult
        , either (Just . InvalidAge) (const Nothing) ageResult
        ]

formatValidationErrors :: [ValidationError] -> String
formatValidationErrors errs = "Validation failed:\n" ++ intercalate "\n" (map show errs)

sanitizeInput :: String -> String
sanitizeInput = unwords . words . filter (`notElem` "\t\n\r")

normalizeProfile :: UserProfile -> UserProfile
normalizeProfile profile = profile
    { username = map toLower (username profile)
    , email = map toLower (email profile)
    }

isValidProfile :: UserProfile -> Bool
isValidProfile profile = case createUserProfile (username profile) (email profile) (age profile) of
    Left _ -> False
    Right _ -> True

sampleProfiles :: [UserProfile]
sampleProfiles =
    [ UserProfile "alice" "alice@example.com" 25
    , UserProfile "bob" "bob@test.org" 30
    , UserProfile "charlie" "charlie@domain.net" 42
    ]module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)
module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isSpace)
import Control.Monad (foldM)

type CSVRow = [String]
type CSVData = [CSVRow]

data ValidationError = 
    EmptyRowError Int
  | InvalidColumnCountError Int Int Int
  | NumericFieldError Int Int String
  deriving (Show, Eq)

parseCSV :: String -> Either String CSVData
parseCSV input = 
    if null input
    then Left "Empty input"
    else Right $ map parseRow $ lines input
  where
    parseRow line = splitOnComma line
    splitOnComma = foldr (\c acc -> if c == ',' then []:acc else (c:head acc):tail acc) [[]]

validateCSVData :: CSVData -> Either [ValidationError] CSVData
validateCSVData rows = do
    errors <- foldM validateRow [] (zip [1..] rows)
    if null errors
    then Right rows
    else Left errors
  where
    validateRow acc (rowNum, row) =
        let checks = [checkEmptyRow rowNum row, checkColumnCount rowNum row, checkNumericFields rowNum row]
            newErrors = concatMap (\f -> f row) checks
        in Right (acc ++ newErrors)
    
    checkEmptyRow rowNum [] = [EmptyRowError rowNum]
    checkEmptyRow _ _ = []
    
    checkColumnCount rowNum row = 
        if length row /= expectedColumns
        then [InvalidColumnCountError rowNum (length row) expectedColumns]
        else []
      where expectedColumns = 4
    
    checkNumericFields rowNum row = 
        concat $ zipWith (\colNum val -> 
            if colNum `elem` numericColumns && not (all isDigit (filter (not . isSpace) val))
            then [NumericFieldError rowNum colNum val]
            else []
        ) [1..] row
      where numericColumns = [2, 4]

processCSVFile :: String -> Either String CSVData
processCSVFile content = do
    parsed <- parseCSV content
    case validateCSVData parsed of
        Right valid -> Right valid
        Left errors -> Left $ "Validation errors: " ++ showErrors errors
  where
    showErrors errs = intercalate "; " $ map showError errs
    showError (EmptyRowError n) = "Row " ++ show n ++ " is empty"
    showError (InvalidColumnCountError n actual expected) = 
        "Row " ++ show n ++ " has " ++ show actual ++ " columns, expected " ++ show expected
    showError (NumericFieldError n col val) = 
        "Row " ++ show n ++ " column " ++ show col ++ " invalid numeric value: " ++ show val

calculateColumnAverage :: CSVData -> Int -> Either String Double
calculateColumnAverage rows colIndex
    | colIndex < 1 || colIndex > length (head rows) = Left "Invalid column index"
    | otherwise = 
        let values = map (read . (!! (colIndex-1))) rows
        in Right $ fromIntegral (sum values) / fromIntegral (length values)

main :: IO ()
main = do
    let testData = "John,25,Engineer,50000\nJane,30,Manager,75000\nBob,abc,Developer,60000"
    case processCSVFile testData of
        Right validData -> do
            putStrLn "Valid CSV data:"
            mapM_ (putStrLn . intercalate ", ") validData
            case calculateColumnAverage validData 2 of
                Right avg -> putStrLn $ "Average age: " ++ show avg
                Left err -> putStrLn err
        Left err -> putStrLn $ "Error: " ++ errmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers