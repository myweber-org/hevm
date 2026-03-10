module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = not (null xs) && all (\x -> x >= -100 && x <= 100) xs

main :: IO ()
main = do
    let sampleData = [1, -2, 3, 0, 5, -8]
    if validateInput sampleData
        then print $ processData sampleData
        else putStrLn "Invalid input data"module DataProcessor where

import Data.Char (isDigit, isAlpha)

-- Validate that a string contains only digits
validateNumeric :: String -> Bool
validateNumeric = all isDigit

-- Validate that a string contains only alphabetic characters
validateAlpha :: String -> Bool
validateAlpha = all isAlpha

-- Transform a string to uppercase
toUpperString :: String -> String
toUpperString = map toUpper
  where
    toUpper c
        | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
        | otherwise = c

-- Process a list of strings: validate and transform
processData :: [String] -> [String]
processData = map toUpperString . filter validateAlpha

-- Calculate statistics on numeric strings
calculateStats :: [String] -> (Int, Double)
calculateStats strs = (count, avg)
  where
    nums = map read (filter validateNumeric strs) :: [Double]
    count = length nums
    avg = if count > 0 then sum nums / fromIntegral count else 0.0

-- Safe division with error handling
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- Main processing pipeline
processPipeline :: [String] -> IO ()
processPipeline input = do
    putStrLn "Processing data..."
    let cleaned = processData input
    putStrLn $ "Cleaned data: " ++ show cleaned
    
    let stats = calculateStats input
    putStrLn $ "Numeric stats - Count: " ++ show (fst stats) ++ ", Average: " ++ show (snd stats)
    
    case safeDivide (snd stats) (fromIntegral $ fst stats) of
        Just val -> putStrLn $ "Average per valid numeric: " ++ show val
        Nothing -> putStrLn "No valid numeric data for division"