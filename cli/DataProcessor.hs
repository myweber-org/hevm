
module DataProcessor where

import Data.Char (toLower, isAlpha, isSpace)
import Data.List (intercalate)
import Data.Maybe (catMaybes)

type Username = String
type Email = String
type Age = Int
type UserProfile = (Username, Email, Age)

-- Validate username: only letters, 3-20 characters
validateUsername :: Username -> Maybe Username
validateUsername username
    | length username < 3 = Nothing
    | length username > 20 = Nothing
    | not (all isAlpha username) = Nothing
    | otherwise = Just username

-- Validate email: simple format check
validateEmail :: Email -> Maybe Email
validateEmail email
    | '@' `notElem` email = Nothing
    | '.' `notElem` (dropWhile (/= '@') email) = Nothing
    | any isSpace email = Nothing
    | otherwise = Just email

-- Validate age: between 1 and 120
validateAge :: Age -> Maybe Age
validateAge age
    | age < 1 = Nothing
    | age > 120 = Nothing
    | otherwise = Just age

-- Create user profile with validation
createUserProfile :: Username -> Email -> Age -> Maybe UserProfile
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

-- Normalize username to lowercase
normalizeUsername :: Username -> Username
normalizeUsername = map toLower

-- Process multiple user profiles
processProfiles :: [UserProfile] -> [String]
processProfiles = map formatProfile

-- Filter valid profiles from raw input
filterValidProfiles :: [Username] -> [Email] -> [Age] -> [UserProfile]
filterValidProfiles usernames emails ages =
    catMaybes $ zipWith3 createUserProfile usernames emails ages

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
    let maybeProfile = createUserProfile "JohnDoe" "john@example.com" 30
    case maybeProfile of
        Just profile -> putStrLn $ formatProfile profile
        Nothing -> putStrLn "Invalid profile data"