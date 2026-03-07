module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [1, -2, 3, 0, 5, -8]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

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
    | null emailStr = Left $ InvalidEmail "Email cannot be empty"
    | '@' `notElem` emailStr = Left $ InvalidEmail "Email must contain @ symbol"
    | '.' `notElem` (dropWhile (/= '@') emailStr) = Left $ InvalidEmail "Email must contain domain with dot"
    | otherwise = Right (map toLower emailStr)

validateAge :: Age -> Either ValidationError Age
validateAge ageVal
    | ageVal < 0 = Left $ InvalidAge "Age cannot be negative"
    | ageVal < 13 = Left $ InvalidAge "User must be at least 13 years old"
    | ageVal > 120 = Left $ InvalidAge "Age must be realistic (<= 120)"
    | otherwise = Right ageVal

createUserProfile :: Username -> Email -> Age -> Either ValidationError UserProfile
createUserProfile name emailStr ageVal = do
    validName <- validateUsername name
    validEmail <- validateEmail emailStr
    validAge <- validateAge ageVal
    return $ UserProfile validName validEmail validAge

normalizeProfile :: UserProfile -> UserProfile
normalizeProfile profile = profile
    { username = map toLower (username profile)
    , email = map toLower (email profile)
    }

formatProfile :: UserProfile -> String
formatProfile profile = intercalate " | "
    [ "User: " ++ username profile
    , "Email: " ++ email profile
    , "Age: " ++ show (age profile)
    ]

isAdult :: UserProfile -> Bool
isAdult = (>= 18) . age

processUserInput :: String -> String -> String -> Either ValidationError String
processUserInput name emailStr ageStr = do
    let ageVal = read ageStr :: Age
    profile <- createUserProfile name emailStr ageVal
    let normalized = normalizeProfile profile
    return $ formatProfile normalized

safeProcessUserInput :: String -> String -> String -> String
safeProcessUserInput name emailStr ageStr =
    case processUserInput name emailStr ageStr of
        Left err -> "Error: " ++ show err
        Right result -> "Success: " ++ result