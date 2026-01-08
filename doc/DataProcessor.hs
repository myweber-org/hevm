
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
    | length name < 3 = Left $ InvalidUsername "Username must be at least 3 characters"
    | length name > 20 = Left $ InvalidUsername "Username must not exceed 20 characters"
    | not (all isValidUsernameChar name) = Left $ InvalidUsername "Username contains invalid characters"
    | otherwise = Right (normalizeUsername name)
    where
        isValidUsernameChar c = isAlpha c || c == '_' || c == '-'
        normalizeUsername = map toLower

validateEmail :: Email -> Either ValidationError Email
validateEmail emailStr
    | '@' `notElem` emailStr = Left $ InvalidEmail "Email must contain @ symbol"
    | '.' `notElem` (dropWhile (/= '@') emailStr) = Left $ InvalidEmail "Email must contain domain with dot"
    | length emailStr > 254 = Left $ InvalidEmail "Email too long"
    | otherwise = Right (map toLower emailStr)

validateAge :: Age -> Either ValidationError Age
validateAge a
    | a < 0 = Left $ InvalidAge "Age cannot be negative"
    | a > 150 = Left $ InvalidAge "Age must be realistic"
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
            [ either (Just . (:[])) (const Nothing) usernameResult
            , either (Just . (:[])) (const Nothing) emailResult
            , either (Just . (:[])) (const Nothing) ageResult
            ]

formatValidationErrors :: [ValidationError] -> String
formatValidationErrors errs = "Validation failed:\n" ++ intercalate "\n" (map show errs)

sanitizeInput :: String -> String
sanitizeInput = unwords . words . filter (/= '\0')

processUserInput :: String -> String -> String -> IO ()
processUserInput rawUsername rawEmail rawAge = do
    let cleanUsername = sanitizeInput rawUsername
    let cleanEmail = sanitizeInput rawEmail
    let age = read rawAge :: Age
    
    case createUserProfile cleanUsername cleanEmail age of
        Left errors -> putStrLn $ formatValidationErrors errors
        Right profile -> putStrLn $ "Created profile: " ++ show profile

sampleProfiles :: [UserProfile]
sampleProfiles =
    [ UserProfile "john_doe" "john@example.com" 30
    , UserProfile "jane-smith" "jane@domain.org" 25
    , UserProfile "admin" "admin@system.local" 35
    ]

filterByAgeRange :: Int -> Int -> [UserProfile] -> [UserProfile]
filterByAgeRange minAge maxAge = filter (\p -> age p >= minAge && age p <= maxAge)

extractEmails :: [UserProfile] -> [Email]
extractEmails = map email

averageAge :: [UserProfile] -> Double
averageAge profiles = fromIntegral (sum ages) / fromIntegral (length ages)
    where ages = map age profiles
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)