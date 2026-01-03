module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubled :: [Int] -> Int
sumPositiveDoubled = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData xs = all (> 0) xs && length xs > 3

main :: IO ()
main = do
    let sampleData = [1, -2, 3, 4, -5, 6]
    putStrLn $ "Original data: " ++ show sampleData
    putStrLn $ "Processed data: " ++ show (processData sampleData)
    putStrLn $ "Validation result: " ++ show (validateData sampleData)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (/= 0) xs then Just xs else Nothing

main :: IO ()
main = do
    let sampleData = [1, -2, 3, 0, 5, -8]
    case validateInput sampleData of
        Just validData -> print $ processData validData
        Nothing -> putStrLn "Invalid input: contains zero"module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

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

-- Example usage with test data
testData :: [Double]
testData = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]

main :: IO ()
main = do
    putStrLn "Testing moving average with window size 3:"
    print $ movingAverage 3 testData
    putStrLn "\nTesting moving average with window size 5:"
    print $ movingAverage 5 testData
module DataProcessor where

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

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -100) xs then Just xs else Nothing
module DataProcessor where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)
import Text.Read (readMaybe)

-- | Safely parse an integer from a string
safeParseInt :: String -> Maybe Int
safeParseInt str = readMaybe (filter (not . isSpace) str)

-- | Validate email format (basic check)
isValidEmail :: String -> Bool
isValidEmail email =
    let parts = split '@' email
    in length parts == 2 &&
       not (null (head parts)) &&
       '.' `elem` (last parts)
  where
    split delimiter = foldr (\c acc -> if c == delimiter then [] : acc else (c : head acc) : tail acc) [[]]

-- | Normalize phone number by removing non-digit characters
normalizePhone :: String -> String
normalizePhone = filter isDigit

-- | Process a list of raw strings into validated integers
processNumbers :: [String] -> [Int]
processNumbers = catMaybes . map safeParseInt

-- | Format a list of items as a comma-separated string
formatList :: [String] -> String
formatList items = intercalate ", " (filter (not . null) items)

-- | Calculate statistics from a list of numbers
data Stats = Stats
    { count :: Int
    , sumTotal :: Int
    , average :: Double
    , minVal :: Maybe Int
    , maxVal :: Maybe Int
    } deriving (Show, Eq)

calculateStats :: [Int] -> Stats
calculateStats [] = Stats 0 0 0.0 Nothing Nothing
calculateStats xs =
    let cnt = length xs
        total = sum xs
        avg = fromIntegral total / fromIntegral cnt
        mn = if null xs then Nothing else Just (minimum xs)
        mx = if null xs then Nothing else Just (maximum xs)
    in Stats cnt total avg mn mx

-- | Safe division with error handling
safeDivide :: Int -> Int -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (fromIntegral x / fromIntegral y)

-- | Main processing pipeline example
processDataPipeline :: [String] -> (Stats, String)
processDataPipeline rawNumbers =
    let numbers = processNumbers rawNumbers
        stats = calculateStats numbers
        formatted = formatList (map show numbers)
    in (stats, "Processed numbers: " ++ formatted)
module DataProcessor where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)

-- Safe integer parsing with error handling
safeReadInt :: String -> Maybe Int
safeReadInt s
  | all isDigit s = Just (read s)
  | otherwise = Nothing

-- Trim whitespace from both ends of a string
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- Validate and clean a list of numeric strings
cleanNumericData :: [String] -> [Int]
cleanNumericData = catMaybes . map (safeReadInt . trim)

-- Calculate statistics from numeric data
data Stats = Stats
  { count :: Int
  , sumTotal :: Int
  , average :: Double
  , validEntries :: [Int]
  } deriving (Show, Eq)

computeStats :: [String] -> Stats
computeStats rawData =
  let cleaned = cleanNumericData rawData
      cnt = length cleaned
      total = sum cleaned
      avg = if cnt > 0 then fromIntegral total / fromIntegral cnt else 0.0
   in Stats cnt total avg cleaned

-- Format statistics as a readable report
formatReport :: Stats -> String
formatReport stats =
  intercalate "\n"
    [ "Data Analysis Report"
    , "===================="
    , "Valid entries: " ++ show (count stats)
    , "Total sum: " ++ show (sumTotal stats)
    , "Average: " ++ show (average stats)
    , "Values: " ++ show (validEntries stats)
    ]

-- Example transformation pipeline
processData :: [String] -> String
processData = formatReport . computeStats
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubled :: [Int] -> Int
sumPositiveDoubled = sum . processNumbers