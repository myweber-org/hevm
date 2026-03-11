module DataProcessor where

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
    where
        windows :: Int -> [a] -> [[a]]
        windows m = takeWhile ((== m) . length) . map (take m) . iterate tail
        
        average :: [Double] -> Double
        average ys = sum ys / fromIntegral (length ys)

smoothData :: [Double] -> [Double]
smoothData = movingAverage 3

calculateTrend :: [Double] -> Maybe Double
calculateTrend [] = Nothing
calculateTrend xs = Just (last xs - head xs)module DataProcessor where

import Data.Char (toLower, isAlpha, isSpace)
import Data.List (intercalate)

type Username = String
type Email = String
type UserProfile = (Username, Email, Int)

validateUsername :: Username -> Maybe Username
validateUsername name
    | length name >= 3 && length name <= 20 &&
      all (\c -> isAlpha c || c == '_' || c == '-') name = Just name
    | otherwise = Nothing

normalizeEmail :: Email -> Email
normalizeEmail = map toLower

validateAge :: Int -> Maybe Int
validateAge age
    | age >= 13 && age <= 120 = Just age
    | otherwise = Nothing

createUserProfile :: Username -> Email -> Int -> Maybe UserProfile
createUserProfile username email age = do
    validName <- validateUsername username
    validAge <- validateAge age
    return (validName, normalizeEmail email, validAge)

formatProfile :: UserProfile -> String
formatProfile (username, email, age) =
    intercalate " | " ["User: " ++ username, "Email: " ++ email, "Age: " ++ show age]

processUserInput :: String -> String -> String -> Maybe String
processUserInput username email ageStr = do
    age <- readMaybe ageStr
    profile <- createUserProfile username email age
    return $ formatProfile profile
  where
    readMaybe :: String -> Maybe Int
    readMaybe s = case reads s of
        [(n, "")] -> Just n
        _ -> Nothing

sanitizeInput :: String -> String
sanitizeInput = unwords . words . filter (\c -> not (c == '\n' || c == '\r'))

main :: IO ()
main = do
    putStrLn "Enter username:"
    username <- sanitizeInput <$> getLine
    putStrLn "Enter email:"
    email <- sanitizeInput <$> getLine
    putStrLn "Enter age:"
    age <- sanitizeInput <$> getLine
    
    case processUserInput username email age of
        Just result -> putStrLn $ "Valid profile created: " ++ result
        Nothing -> putStrLn "Invalid input. Please check username (3-20 alphanumeric chars) and age (13-120)."module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenNumbers :: [Int] -> [Int]
processEvenNumbers = 
    filterAndTransform even (\x -> x * 2 + 1)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processEvenNumbers

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processEvenNumbers numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)module DataProcessor where

import Data.List.Split (splitOn)

parseCSV :: String -> [[Double]]
parseCSV content = map (map read . splitOn ",") $ lines content

calculateAverages :: [[Double]] -> [Double]
calculateAverages rows = 
    if null rows then []
    else map average $ transpose rows
  where
    average xs = sum xs / fromIntegral (length xs)
    transpose = foldr (zipWith (:)) (repeat [])

processCSVData :: String -> [Double]
processCSVData = calculateAverages . parseCSVmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processNumbers input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)