module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform even (* 2)

main :: IO ()
main = do
    let input = [1..10]
    let result = processData input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbers
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
    | a > 150 = Left $ InvalidAge "Age must be realistic (≤150)"
    | a < 13 = Left $ InvalidAge "User must be at least 13 years old"
    | otherwise = Right a

createUserProfile :: Username -> Email -> Age -> Either [ValidationError] UserProfile
createUserProfile name emailStr ageVal = case results of
    [] -> Right $ UserProfile validName validEmail validAge
    errs -> Left errs
  where
    nameResult = validateUsername name
    emailResult = validateEmail emailStr
    ageResult = validateAge ageVal
    
    results = catMaybes
        [ either (Just . InvalidUsername) (const Nothing) nameResult
        , either (Just . InvalidEmail) (const Nothing) emailResult
        , either (Just . InvalidAge) (const Nothing) ageResult
        ]
    
    validName = either (const "") id nameResult
    validEmail = either (const "") id emailResult
    validAge = either (const 0) id ageResult

formatValidationErrors :: [ValidationError] -> String
formatValidationErrors errs = intercalate "\n" $ map formatError errs
  where
    formatError (InvalidUsername msg) = "Username error: " ++ msg
    formatError (InvalidEmail msg) = "Email error: " ++ msg
    formatError (InvalidAge msg) = "Age error: " ++ msg

sanitizeUserInput :: String -> String
sanitizeUserInput = unwords . words . filter (/= '\0')

processUserBatch :: [(Username, Email, Age)] -> [Either [ValidationError] UserProfile]
processUserBatch = map (\(n, e, a) -> createUserProfile n e a)

validProfilesCount :: [Either [ValidationError] UserProfile] -> Int
validProfilesCount profiles = length [p | Right p <- profiles]