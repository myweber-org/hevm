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

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessed :: (Int -> Int) -> [Int] -> Int
sumProcessed f = foldl' (\acc x -> acc + f x) 0

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Even squares: " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of doubled values: " ++ show (sumProcessed (*2) numbers)module DataProcessor where

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

safeMovingAverage :: Int -> [Double] -> Maybe [Double]
safeMovingAverage n xs
    | n <= 0 = Nothing
    | otherwise = Just $ movingAverage n xs

testData :: [Double]
testData = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]module DataProcessor where

import Data.Char (toLower, isAlpha, isSpace)
import Data.List (intercalate)

type Username = String
type Email = String
type UserProfile = (Username, Email, Int)

validateUsername :: Username -> Maybe Username
validateUsername name
    | length name >= 3 && length name <= 20 && all isAlpha name = Just name
    | otherwise = Nothing

normalizeEmail :: Email -> Email
normalizeEmail = map toLower . filter (not . isSpace)

validateEmail :: Email -> Maybe Email
validateEmail email
    | '@' `elem` email && '.' `elem` email = Just (normalizeEmail email)
    | otherwise = Nothing

createProfile :: Username -> Email -> Int -> Maybe UserProfile
createProfile username email age = do
    validName <- validateUsername username
    validEmail <- validateEmail email
    if age >= 0 && age <= 150
        then Just (validName, validEmail, age)
        else Nothing

formatProfile :: UserProfile -> String
formatProfile (name, email, age) =
    intercalate " | " ["Username: " ++ name, "Email: " ++ email, "Age: " ++ show age]

processUserData :: [String] -> [String]
processUserData inputs = 
    mapMaybe processSingle inputs
    where
        processSingle input =
            case words input of
                [name, email, ageStr] ->
                    case reads ageStr of
                        [(age, "")] -> createProfile name email age >>= Just . formatProfile
                        _ -> Nothing
                _ -> Nothing

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) =
    case f x of
        Just y -> y : mapMaybe f xs
        Nothing -> mapMaybe f xsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [1, -2, 3, -4, 5]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isAlpha)

type CSVRow = [String]
type ValidationError = String

validateCSVRow :: CSVRow -> Either ValidationError CSVRow
validateCSVRow [] = Left "Empty row"
validateCSVRow row
    | length row /= 3 = Left $ "Expected 3 columns, got " ++ show (length row)
    | not (all validField row) = Left "Invalid characters in fields"
    | not (validId (row !! 0)) = Left "Invalid ID format"
    | not (validName (row !! 1)) = Left "Invalid name format"
    | not (validAmount (row !! 2)) = Left "Invalid amount format"
    | otherwise = Right row
  where
    validField = all (\c -> isAlpha c || isDigit c || c `elem` " -_.")
    validId str = not (null str) && all isDigit str && length str == 6
    validName str = not (null str) && length str <= 50 && all isAlpha (filter (/= ' ') str)
    validAmount str = case reads str :: [(Double, String)] of
        [(n, "")] -> n >= 0 && n <= 1000000
        _ -> False

parseCSV :: String -> Either ValidationError [CSVRow]
parseCSV content = 
    let rows = map (splitOn ',') (lines content)
        validated = map validateCSVRow rows
    in case partitionEithers validated of
        ([], validRows) -> Right validRows
        (errors, _) -> Left $ "Validation errors:\n" ++ intercalate "\n" (take 3 errors)

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delimiter str = 
    let (token, rest) = break (== delimiter) str
    in token : case rest of
        [] -> []
        (_:xs) -> splitOn delimiter xs

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr (either left right) ([], [])
  where
    left a (as, bs) = (a:as, bs)
    right b (as, bs) = (as, b:bs)

processCSVData :: String -> IO ()
processCSVData filename = do
    content <- readFile filename
    case parseCSV content of
        Left err -> putStrLn $ "Error: " ++ err
        Right rows -> do
            putStrLn $ "Successfully processed " ++ show (length rows) ++ " rows"
            let total = sum [read (row !! 2) | row <- rows]
            putStrLn $ "Total amount: " ++ show total