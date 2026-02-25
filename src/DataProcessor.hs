
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData = all (> 0)

main :: IO ()
main = do
    let inputData = [1, -2, 3, 0, 5, -8]
    let processed = processData inputData
    putStrLn $ "Original data: " ++ show inputData
    putStrLn $ "Processed data: " ++ show processed
    putStrLn $ "Data validation: " ++ show (validateData processed)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processEvenSquares

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)
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
    | length name > 20 = Left $ InvalidUsername "Username must not exceed 20 characters"
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
    | otherwise = Right a

createUserProfile :: Username -> Email -> Age -> Either [ValidationError] UserProfile
createUserProfile name emailStr ageVal = case results of
    [] -> Right UserProfile
        { username = validatedName
        , email = validatedEmail
        , age = validatedAge
        }
    errs -> Left errs
  where
    nameResult = validateUsername name
    emailResult = validateEmail emailStr
    ageResult = validateAge ageVal
    
    (validatedName, validatedEmail, validatedAge) = case (nameResult, emailResult, ageResult) of
        (Right n, Right e, Right a) -> (n, e, a)
        _ -> ("", "", 0)
    
    results = catMaybes
        [ either (Just . InvalidUsername) (const Nothing) nameResult
        , either (Just . InvalidEmail) (const Nothing) emailResult
        , either (Just . InvalidAge) (const Nothing) ageResult
        ]

formatErrors :: [ValidationError] -> String
formatErrors errs = "Validation failed:\n" ++ intercalate "\n" (map show errs)

sanitizeInput :: String -> String
sanitizeInput = unwords . words

processUserData :: String -> String -> String -> IO ()
processUserData rawName rawEmail rawAge = do
    let cleanName = sanitizeInput rawName
    let cleanEmail = sanitizeInput rawEmail
    case reads rawAge of
        [(ageNum, "")] -> case createUserProfile cleanName cleanEmail ageNum of
            Right profile -> putStrLn $ "Successfully created profile: " ++ show profile
            Left errs -> putStrLn $ formatErrors errs
        _ -> putStrLn "Invalid age format. Please provide a number."

sampleUsers :: [UserProfile]
sampleUsers =
    [ UserProfile "alice" "alice@example.com" 30
    , UserProfile "bob" "bob@test.org" 25
    , UserProfile "charlie" "charlie@domain.net" 35
    ]module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, 0, 5, -8]
    let result = processNumbers input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processDatamodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [1, -2, 3, -4, 5]
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