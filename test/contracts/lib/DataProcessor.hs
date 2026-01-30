
module DataProcessor where

import Data.Char (toLower, isAlpha, isSpace)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

type Username = String
type Email = String
type Age = Int

data UserProfile = UserProfile
    { username :: Username
    , email :: Email
    , age :: Age
    } deriving (Show, Eq)

data ValidationError = InvalidUsername String
                     | InvalidEmail String
                     | InvalidAge String
                     deriving (Show, Eq)

normalizeUsername :: String -> String
normalizeUsername = filter isAlpha . map toLower

validateUsername :: String -> Either ValidationError Username
validateUsername name
    | length normalized < 3 = Left $ InvalidUsername "Username must be at least 3 characters"
    | length normalized > 20 = Left $ InvalidUsername "Username must not exceed 20 characters"
    | otherwise = Right normalized
    where normalized = normalizeUsername name

validateEmail :: String -> Either ValidationError Email
validateEmail emailStr
    | '@' `notElem` emailStr = Left $ InvalidEmail "Email must contain @ symbol"
    | '.' `notElem` (dropWhile (/= '@') emailStr) = Left $ InvalidEmail "Email must contain domain with dot"
    | length emailStr > 254 = Left $ InvalidEmail "Email too long"
    | otherwise = Right emailStr

validateAge :: Int -> Either ValidationError Age
validateAge a
    | a < 0 = Left $ InvalidAge "Age cannot be negative"
    | a > 150 = Left $ InvalidAge "Age must be realistic"
    | otherwise = Right a

createUserProfile :: String -> String -> Int -> Either [ValidationError] UserProfile
createUserProfile name emailStr ageVal = case results of
    [] -> Right $ UserProfile validName validEmail validAge
    errs -> Left errs
    where
        nameResult = validateUsername name
        emailResult = validateEmail emailStr
        ageResult = validateAge ageVal
        
        results = lefts [nameResult, emailResult, ageResult]
        
        validName = either (const "") id nameResult
        validEmail = either (const "") id emailResult
        validAge = either (const 0) id ageResult

lefts :: [Either a b] -> [a]
lefts = foldr (\x acc -> case x of Left err -> err : acc; _ -> acc) []

formatErrors :: [ValidationError] -> String
formatErrors errs = "Validation failed:\n" ++ intercalate "\n" (map show errs)

sanitizeInput :: String -> String
sanitizeInput = unwords . words . filter (\c -> isAlpha c || isSpace c || c `elem` "._-@")

processUserData :: String -> String -> Int -> IO ()
processUserData rawName rawEmail rawAge = do
    let cleanName = sanitizeInput rawName
    let cleanEmail = sanitizeInput rawEmail
    
    case createUserProfile cleanName cleanEmail rawAge of
        Left errs -> putStrLn $ formatErrors errs
        Right profile -> do
            putStrLn "User profile created successfully:"
            print profile
            putStrLn $ "Welcome, " ++ username profile ++ "!"
module DataProcessor where

import Data.Char (isDigit, toUpper)
import Data.List (intercalate)

-- Validate if a string contains only digits
validateNumeric :: String -> Bool
validateNumeric = all isDigit

-- Transform a string to uppercase
transformToUpper :: String -> String
transformToUpper = map toUpper

-- Process a list of strings: validate numeric, transform to uppercase
processData :: [String] -> [String]
processData = map transformToUpper . filter validateNumeric

-- Format processed data as a comma-separated string
formatOutput :: [String] -> String
formatOutput = intercalate ", "

-- Main processing pipeline
runDataProcessor :: [String] -> String
runDataProcessor = formatOutput . processData