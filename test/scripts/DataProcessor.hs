
module DataProcessor where

import Data.Char (isAlpha, isDigit, toLower)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

type Username = String
type Email = String
type Age = Int
type UserProfile = (Username, Email, Age)

validateUsername :: Username -> Maybe Username
validateUsername username
    | length username >= 3 && length username <= 20 &&
      all (\c -> isAlpha c || isDigit c || c `elem` "_-") username = Just username
    | otherwise = Nothing

validateEmail :: Email -> Maybe Email
validateEmail email
    | '@' `elem` email && '.' `elem` (dropWhile (/= '@') email) = Just email
    | otherwise = Nothing

validateAge :: Age -> Maybe Age
validateAge age
    | age >= 13 && age <= 120 = Just age
    | otherwise = Nothing

createUserProfile :: Username -> Email -> Age -> Maybe UserProfile
createUserProfile username email age = do
    validUsername <- validateUsername username
    validEmail <- validateEmail email
    validAge <- validateAge age
    return (validUsername, validEmail, validAge)

normalizeUsername :: Username -> Username
normalizeUsername = map toLower . filter (\c -> isAlpha c || isDigit c)

formatProfile :: UserProfile -> String
formatProfile (username, email, age) =
    intercalate " | " ["User: " ++ username, "Email: " ++ email, "Age: " ++ show age]

processUserInput :: String -> String -> String -> String
processUserInput username email ageStr =
    case createUserProfile username email (read ageStr) of
        Just profile -> "Valid profile: " ++ formatProfile profile
        Nothing -> "Invalid input data provided"

sampleProfiles :: [UserProfile]
sampleProfiles = 
    [ ("john_doe", "john@example.com", 25)
    , ("alice42", "alice@test.org", 30)
    , ("bob-smith", "bob@company.net", 28)
    ]module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processDatamodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show result