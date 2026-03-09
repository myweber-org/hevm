module DataProcessor where

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows :: Int -> [a] -> [[a]]
    windows m = takeWhile ((== m) . length) . map (take m) . tails
    
    average :: [Double] -> Double
    average ys = sum ys / fromIntegral (length ys)

-- Helper function from Data.List
tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_:ys) = xs : tails ysmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)
module DataProcessor where

import Data.Time
import Data.Time.Format
import Data.List
import Data.Char
import System.Locale

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

data Record = Record
    { recordId :: Int
    , recordDate :: Day
    , recordValue :: Double
    , recordCategory :: String
    } deriving (Show, Eq)

parseRecord :: [String] -> Maybe Record
parseRecord [idStr, dateStr, valueStr, category]
    | all isDigit idStr
    , Just date <- parseDate dateStr
    , all (\c -> isDigit c || c == '.') valueStr
    , not (null category) =
        Just $ Record (read idStr) date (read valueStr) category
parseRecord _ = Nothing

filterRecordsByDate :: Day -> Day -> [Record] -> [Record]
filterRecordsByDate startDate endDate =
    filter (\r -> recordDate r >= startDate && recordDate r <= endDate)

calculateTotalByCategory :: [Record] -> [(String, Double)]
calculateTotalByCategory records =
    map (\cat -> (cat, sum [v | r <- records, recordCategory r == cat, let v = recordValue r]))
    $ nub $ map recordCategory records

processCSVData :: String -> Day -> Day -> Either String [(String, Double)]
processCSVData csvContent startDate endDate = do
    let lines' = drop 1 $ lines csvContent
    let records = map (parseRecord . splitByComma) lines'
    
    if any isNothing records
        then Left "Invalid CSV format"
        else Right $ calculateTotalByCategory $
             filterRecordsByDate startDate endDate $
             catMaybes records

splitByComma :: String -> [String]
splitByComma = foldr f [""]
  where
    f ',' (x:xs) = "":x:xs
    f c (x:xs) = (c:x):xs
    f _ [] = []

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\x acc -> case x of
    Just val -> val : acc
    Nothing -> acc) []module DataProcessor where

import Data.Char (isDigit, isAlpha, toUpper)
import Data.List (intercalate)

-- Validate that a string contains only alphabetic characters
validateAlpha :: String -> Maybe String
validateAlpha s
    | all isAlpha s = Just s
    | otherwise = Nothing

-- Validate that a string contains only digits
validateDigits :: String -> Maybe String
validateDigits s
    | all isDigit s = Just s
    | otherwise = Nothing

-- Transform string to uppercase
toUppercase :: String -> String
toUppercase = map toUpper

-- Normalize phone number by removing non-digit characters
normalizePhone :: String -> String
normalizePhone = filter isDigit

-- Format name as "Last, First"
formatName :: String -> String -> String
formatName first last = last ++ ", " ++ first

-- Process user data with validation and transformation
processUserData :: String -> String -> String -> Maybe (String, String, String)
processUserData firstName lastName phone = do
    validFirst <- validateAlpha firstName
    validLast <- validateAlpha lastName
    validPhone <- validateDigits (normalizePhone phone)
    
    let formattedName = formatName (toUppercase validFirst) (toUppercase validLast)
    let formattedPhone = intercalate "-" $ chunksOf 3 validPhone
    
    return (formattedName, formattedPhone, show (length validPhone))

-- Helper function to split string into chunks
chunksOf :: Int -> String -> [String]
chunksOf _ [] = []
chunksOf n s = take n s : chunksOf n (drop n s)

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
    let result = processUserData "John" "Doe" "1234567890"
    case result of
        Just (name, phone, length) -> 
            putStrLn $ "Processed: " ++ name ++ " | " ++ phone ++ " | Length: " ++ length
        Nothing -> 
            putStrLn "Invalid input data"module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows m ys = take (length ys - m + 1) $ zipWith (++) (tails ys) (repeat [])
    average zs = sum zs / fromIntegral (length zs)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)module DataProcessor where

import Data.List (tails)

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map avg $ filter (\w -> length w == n) $ tails xs
  where
    avg window = sum window / fromIntegral n

-- Example usage with test data
testData :: [Double]
testData = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]

main :: IO ()
main = do
    putStrLn "Testing moving average with window size 3:"
    print $ movingAverage 3 testData
    putStrLn "\nTesting moving average with window size 5:"
    print $ movingAverage 5 testDatamodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processData

validateInput :: [Int] -> Maybe [Int]
validateInput xs = 
    if all (\x -> x >= -100 && x <= 100) xs
        then Just xs
        else Nothing

main :: IO ()
main = do
    let sampleData = [-5, 2, 0, 8, -1, 10]
    case validateInput sampleData of
        Just validData -> do
            putStrLn $ "Original data: " ++ show validData
            putStrLn $ "Processed data: " ++ show (processData validData)
            putStrLn $ "Sum of positive doubles: " ++ show (sumPositiveDoubles validData)
        Nothing -> putStrLn "Input validation failed: values out of range"module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

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
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -100) xs then Just xs else Nothing

main :: IO ()
main = do
    let sampleData = [1, -5, 3, 0, 8, -2]
    case validateInput sampleData of
        Just validData -> print $ processData validData
        Nothing -> putStrLn "Invalid input data"