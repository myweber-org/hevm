module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

import Data.Char (toLower, isAlpha, isSpace)
import Data.List (intercalate)

type Username = String
type Email = String
type UserProfile = (Username, Email, Int)

-- Validate username: only letters, 3-20 characters
validateUsername :: Username -> Either String Username
validateUsername username
    | length username < 3 = Left "Username must be at least 3 characters"
    | length username > 20 = Left "Username cannot exceed 20 characters"
    | not (all isAlpha username) = Left "Username must contain only letters"
    | otherwise = Right username

-- Normalize email: lowercase and trim spaces
normalizeEmail :: Email -> Email
normalizeEmail = map toLower . trim
  where
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Validate email: simple format check
validateEmail :: Email -> Either String Email
validateEmail email
    | '@' `notElem` email = Left "Email must contain @ symbol"
    | '.' `notElem` (dropWhile (/= '@') email) = Left "Email must contain domain"
    | otherwise = Right (normalizeEmail email)

-- Validate age: must be between 13 and 120
validateAge :: Int -> Either String Int
validateAge age
    | age < 13 = Left "Age must be at least 13"
    | age > 120 = Left "Age must be 120 or less"
    | otherwise = Right age

-- Create validated user profile
createUserProfile :: Username -> Email -> Int -> Either String UserProfile
createUserProfile username email age = do
    validUsername <- validateUsername username
    validEmail <- validateEmail email
    validAge <- validateAge age
    return (validUsername, validEmail, validAge)

-- Format user profile for display
formatProfile :: UserProfile -> String
formatProfile (username, email, age) =
    intercalate "\n"
        [ "Username: " ++ username
        , "Email: " ++ email
        , "Age: " ++ show age
        ]

-- Process multiple user profiles
processProfiles :: [UserProfile] -> [String]
processProfiles = map formatProfile . filter (\(_, _, age) -> age >= 18)

-- Example usage
exampleProfile :: Either String UserProfile
exampleProfile = createUserProfile "JohnDoe" "john@example.com" 25

exampleInvalidProfile :: Either String UserProfile
exampleInvalidProfile = createUserProfile "J0hn" "invalid-email" 10