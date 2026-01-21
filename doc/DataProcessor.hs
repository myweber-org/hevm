module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just xmodule DataProcessor where

import Data.Char (isDigit, isAlpha, toUpper)

-- Validate if a string contains only digits
validateDigits :: String -> Bool
validateDigits = all isDigit

-- Validate if a string contains only alphabetic characters
validateAlpha :: String -> Bool
validateAlpha = all isAlpha

-- Transform string to uppercase
toUppercase :: String -> String
toUppercase = map toUpper

-- Process a list of strings with validation and transformation
processData :: [String] -> [String]
processData = map toUppercase . filter validateAlpha

-- Calculate statistics on numeric strings
calculateStats :: [String] -> (Int, Int, Double)
calculateStats strs = 
    let nums = map read (filter validateDigits strs) :: [Int]
        count = length nums
        total = sum nums
        avg = if count > 0 then fromIntegral total / fromIntegral count else 0.0
    in (count, total, avg)

-- Safe head function with Maybe type
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- Main processing pipeline
processPipeline :: [String] -> IO ()
processPipeline input = do
    putStrLn "Processing data..."
    let filtered = processData input
    putStrLn $ "Filtered alphabetic strings: " ++ show filtered
    
    let stats = calculateStats input
    putStrLn $ "Numeric statistics: " ++ show stats
    
    case safeHead filtered of
        Just first -> putStrLn $ "First valid item: " ++ first
        Nothing -> putStrLn "No valid items found"
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)module DataProcessor where

import Data.List (tails)

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map avg $ filter (\window -> length window == n) $ tails xs
  where
    avg window = sum window / fromIntegral n