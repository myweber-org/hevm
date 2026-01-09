module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let input = [1..10]
    let result = processData input
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let inputData = [1, -2, 3, 0, 5, -8]
    let result = processData inputData
    putStrLn $ "Processed data: " ++ show resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform even (\x -> x * 2 + 1)

validateInput :: [Int] -> Maybe [Int]
validateInput xs
    | null xs = Nothing
    | any (< 0) xs = Nothing
    | otherwise = Just xs

main :: IO ()
main = do
    let sampleData = [1..10]
    case validateInput sampleData of
        Just validData -> do
            putStrLn "Original data:"
            print validData
            putStrLn "Processed data (even numbers doubled and incremented):"
            print $ processData validData
        Nothing -> putStrLn "Invalid input data"
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
  | not (all isValidUsernameChar name) = Left $ InvalidUsername "Username contains invalid characters"
  | otherwise = Right (normalizeUsername name)
  where
    isValidUsernameChar c = isAlpha c || c == '_' || c == '-'
    normalizeUsername = map toLower

validateEmail :: Email -> Either ValidationError Email
validateEmail emailStr
  | '@' `notElem` emailStr = Left $ InvalidEmail "Email must contain @ symbol"
  | '.' `notElem` (dropWhile (/= '@') emailStr) = Left $ InvalidEmail "Email must contain domain with dot"
  | any isSpace emailStr = Left $ InvalidEmail "Email cannot contain spaces"
  | otherwise = Right (map toLower emailStr)

validateAge :: Age -> Either ValidationError Age
validateAge a
  | a < 0 = Left $ InvalidAge "Age cannot be negative"
  | a > 150 = Left $ InvalidAge "Age must be realistic (<= 150)"
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
      [ either (Just . (:[])) (const Nothing) nameResult
      , either (Just . (:[])) (const Nothing) emailResult
      , either (Just . (:[])) (const Nothing) ageResult
      ]
    
    Right validName = nameResult
    Right validEmail = emailResult
    Right validAge = ageResult

formatValidationErrors :: [ValidationError] -> String
formatValidationErrors errs = "Validation failed:\n" ++ intercalate "\n" (map show errs)

sanitizeUserInput :: String -> String
sanitizeUserInput = unwords . words

sampleUsers :: [UserProfile]
sampleUsers =
  [ UserProfile "john_doe" "john@example.com" 30
  , UserProfile "jane-smith" "jane@test.org" 25
  , UserProfile "admin" "admin@system.local" 42
  ]

isAdult :: UserProfile -> Bool
isAdult = (>= 18) . age

getAdultUsernames :: [UserProfile] -> [Username]
getAdultUsernames = map username . filter isAdult