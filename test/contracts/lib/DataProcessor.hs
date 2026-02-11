
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformFunction xs =
    map transformFunction (filter predicate xs)

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processEvenSquares

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (>0) xs then Just xs else Nothing

main :: IO ()
main = do
    let sampleData = [1,2,3,4,5,6]
    case validateInput sampleData of
        Just validData -> do
            putStrLn $ "Original data: " ++ show validData
            putStrLn $ "Processed data: " ++ show (processEvenSquares validData)
            putStrLn $ "Sum of processed data: " ++ show (sumProcessedData validData)
        Nothing -> putStrLn "Invalid input: all numbers must be positive"module DataProcessor where

import Data.List (tails)

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ filter (\window -> length window == n) $ tails xs
  where
    average :: Fractional a => [a] -> a
    average ws = sum ws / fromIntegral (length ws)

smoothData :: Fractional a => Int -> [a] -> [a]
smoothData windowSize dataSeries = movingAverage windowSize dataSeries

calculateTrend :: Fractional a => [a] -> Maybe a
calculateTrend [] = Nothing
calculateTrend [_] = Nothing
calculateTrend values = Just slope
  where
    n = fromIntegral $ length values
    indices = [0..n-1]
    sumX = sum indices
    sumY = sum values
    sumXY = sum $ zipWith (*) indices values
    sumX2 = sum $ map (^2) indices
    slope = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

main :: IO ()
main = do
    let numbers = [-3, 1, 4, -1, 5, 9, -2, 6]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbers
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
    putStrLn $ "Sum of even squares: " ++ 
        show (sumProcessed (\x -> x * x) (filter even numbers))module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)module DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter evenmodule DataProcessor where

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

import Data.Char (isAlpha, isSpace, toLower)
import Data.List (intercalate)
import Data.Maybe (catMaybes)

type Username = String
type Email = String
type Age = Int
type UserProfile = (Username, Email, Age)

validateUsername :: Username -> Maybe Username
validateUsername name
    | length name < 3 = Nothing
    | length name > 20 = Nothing
    | not (all isAlpha name) = Nothing
    | otherwise = Just name

validateEmail :: Email -> Maybe Email
validateEmail email
    | '@' `notElem` email = Nothing
    | '.' `notElem` (dropWhile (/= '@') email) = Nothing
    | any isSpace email = Nothing
    | otherwise = Just email

validateAge :: Age -> Maybe Age
validateAge age
    | age < 0 = Nothing
    | age > 150 = Nothing
    | otherwise = Just age

createUserProfile :: Username -> Email -> Age -> Maybe UserProfile
createUserProfile username email age = do
    validUsername <- validateUsername username
    validEmail <- validateEmail email
    validAge <- validateAge age
    return (validUsername, validEmail, validAge)

normalizeUsername :: Username -> Username
normalizeUsername = map toLower

sanitizeEmail :: Email -> Email
sanitizeEmail = filter (not . isSpace)

formatProfile :: UserProfile -> String
formatProfile (username, email, age) =
    intercalate " | " [username, email, show age]

processUserInputs :: [Username] -> [Email] -> [Age] -> [String]
processUserInputs usernames emails ages =
    let normalizedUsernames = map normalizeUsername usernames
        sanitizedEmails = map sanitizeEmail emails
        profiles = catMaybes $ zipWith3 createUserProfile 
                   normalizedUsernames sanitizedEmails ages
    in map formatProfile profiles

validateAllProfiles :: [UserProfile] -> ([UserProfile], [UserProfile])
validateAllProfiles profiles =
    let validated = map (\(u,e,a) -> createUserProfile u e a) profiles
        valid = catMaybes validated
        invalid = [p | (p, mv) <- zip profiles validated, mv == Nothing]
    in (valid, invalid)