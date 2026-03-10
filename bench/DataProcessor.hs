module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processDatamodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

import Data.List.Split (splitOn)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ",") (lines content)

extractNumericColumn :: CSVData -> Int -> Maybe [Double]
extractNumericColumn dataRows colIndex
    | null dataRows = Nothing
    | otherwise = sequence $ map (safeRead . (!! colIndex)) dataRows
  where
    safeRead s = case reads s of
        [(val, "")] -> Just val
        _ -> Nothing

calculateAverage :: [Double] -> Maybe Double
calculateAverage vals
    | null vals = Nothing
    | otherwise = Just (sum vals / fromIntegral (length vals))

processCSVFile :: String -> Int -> IO (Maybe Double)
processCSVFile filePath columnIndex = do
    content <- readFile filePath
    let parsedData = parseCSV content
    case extractNumericColumn parsedData columnIndex of
        Nothing -> return Nothing
        Just numbers -> return $ calculateAverage numbers

main :: IO ()
main = do
    result <- processCSVFile "data.csv" 2
    case result of
        Nothing -> putStrLn "Failed to process CSV data"
        Just avg -> putStrLn $ "Average: " ++ show avgmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -100) xs then Just xs else Nothingmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print result
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

validateUsername :: Username -> Maybe Username
validateUsername name
  | length name < 3 = Nothing
  | length name > 20 = Nothing
  | not (all isValidUsernameChar name) = Nothing
  | otherwise = Just (normalizeUsername name)
  where
    isValidUsernameChar c = isAlpha c || c == '_' || c == '-'
    normalizeUsername = map toLower

validateEmail :: Email -> Maybe Email
validateEmail emailStr
  | '@' `notElem` emailStr = Nothing
  | '.' `notElem` (dropWhile (/= '@') emailStr) = Nothing
  | any isSpace emailStr = Nothing
  | otherwise = Just (map toLower emailStr)

validateAge :: Age -> Maybe Age
validateAge a
  | a < 0 = Nothing
  | a > 150 = Nothing
  | otherwise = Just a

createUserProfile :: Username -> Email -> Age -> Maybe UserProfile
createUserProfile un em ag = do
  validUsername <- validateUsername un
  validEmail <- validateEmail em
  validAge <- validateAge ag
  return $ UserProfile validUsername validEmail validAge

sanitizeInput :: String -> String
sanitizeInput = unwords . words . filter (/= '\0')

formatUserDisplay :: UserProfile -> String
formatUserDisplay user =
  intercalate " | "
    [ "User: " ++ username user
    , "Email: " ++ email user
    , "Age: " ++ show (age user)
    ]

processUserBatch :: [(Username, Email, Age)] -> [UserProfile]
processUserBatch = catMaybes . map (\(u,e,a) -> createUserProfile u e a)

calculateAverageAge :: [UserProfile] -> Double
calculateAverageAge users =
  if null users
    then 0.0
    else fromIntegral (sum (map age users)) / fromIntegral (length users)