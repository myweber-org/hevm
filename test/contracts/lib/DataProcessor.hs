
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processEvenSquares

main :: IO ()
main = do
    let testData = [1..10]
    putStrLn $ "Original data: " ++ show testData
    putStrLn $ "Processed data: " ++ show (processEvenSquares testData)
    putStrLn $ "Sum of processed data: " ++ show (sumProcessedData testData)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processNumbers input
    print result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-5, 3, 0, 8, -2, 10]
    let result = processNumbers numbers
    print result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessed :: (Int -> Int) -> [Int] -> Int
sumProcessed processor = sum . map processor

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Even squares: " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of doubled evens: " ++ 
        show (sumProcessed (*2) (filter even numbers))module DataProcessor where

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
    putStrLn $ "Even squares: " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of even squares: " ++ show (sumProcessed numbers)module DataProcessor where

import Data.Char (isAlpha, isDigit, toLower)
import Data.List (intercalate)
import Data.Maybe (catMaybes)

data ValidationError = InvalidEmail String
                     | InvalidPhone String
                     | InvalidUsername String
                     deriving (Show, Eq)

data UserProfile = UserProfile
    { username :: String
    , email :: String
    , phone :: String
    , age :: Int
    } deriving (Show)

validateEmail :: String -> Maybe ValidationError
validateEmail email
    | '@' `notElem` email = Just $ InvalidEmail "Missing @ symbol"
    | '.' `notElem` (dropWhile (/= '@') email) = Just $ InvalidEmail "Missing domain separator"
    | length email > 254 = Just $ InvalidEmail "Email too long"
    | otherwise = Nothing

validatePhone :: String -> Maybe ValidationError
validatePhone phone
    | not (all isDigit cleaned) = Just $ InvalidPhone "Contains non-digit characters"
    | length cleaned < 10 = Just $ InvalidPhone "Too short"
    | length cleaned > 15 = Just $ InvalidPhone "Too long"
    | otherwise = Nothing
    where cleaned = filter isDigit phone

validateUsername :: String -> Maybe ValidationError
validateUsername name
    | length name < 3 = Just $ InvalidUsername "Too short"
    | length name > 20 = Just $ InvalidUsername "Too long"
    | not (all isValidChar name) = Just $ InvalidUsername "Contains invalid characters"
    | otherwise = Nothing
    where isValidChar c = isAlpha c || isDigit c || c `elem` "_-"

normalizeEmail :: String -> String
normalizeEmail = map toLower . trim
    where trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

normalizePhone :: String -> String
normalizePhone = filter isDigit

validateProfile :: UserProfile -> [ValidationError]
validateProfile profile = catMaybes
    [ validateUsername (username profile)
    , validateEmail (email profile)
    , validatePhone (phone profile)
    ]

transformProfile :: UserProfile -> UserProfile
transformProfile profile = profile
    { email = normalizeEmail (email profile)
    , phone = normalizePhone (phone profile)
    }

processProfile :: UserProfile -> Either [ValidationError] UserProfile
processProfile profile =
    case validateProfile profile of
        [] -> Right $ transformProfile profile
        errors -> Left errors

formatErrors :: [ValidationError] -> String
formatErrors errors = intercalate "; " $ map show errors

createProfile :: String -> String -> String -> Int -> Either String UserProfile
createProfile name email phone ageVal =
    case processProfile (UserProfile name email phone ageVal) of
        Right profile -> Right profile
        Left errors -> Left $ "Validation failed: " ++ formatErrors errorsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform even (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -100) xs then Just xs else Nothing

main :: IO ()
main = do
    let sampleData = [1, -5, 10, -2, 15]
    case validateInput sampleData of
        Just validData -> do
            putStrLn "Original data:"
            print sampleData
            putStrLn "Processed data (positive numbers doubled):"
            print $ processNumbers validData
        Nothing -> putStrLn "Input contains invalid numbers"
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

sumOfProcessed :: [Int] -> Int
sumOfProcessed = sum . processEvenSquares
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Even squares: " ++ show (processEvenSquares numbers)
    putStrLn $ "First element: " ++ show (safeHead numbers)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [-3, 1, 0, 5, -2, 8]
    let result = processData input
    print result