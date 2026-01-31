
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (^2)

processOddCubes :: [Int] -> [Int]
processOddCubes = filterAndTransform odd (^3)

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original: " ++ show numbers
    putStrLn $ "Even squares: " ++ show (processEvenSquares numbers)
    putStrLn $ "Odd cubes: " ++ show (processOddCubes numbers)module DataProcessor where

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

-- Example usage function
processSampleData :: IO ()
processSampleData = do
    let sampleData = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
    let windowSize = 3
    putStrLn $ "Original data: " ++ show sampleData
    putStrLn $ "Moving average (window=" ++ show windowSize ++ "): " 
             ++ show (movingAverage windowSize sampleData)module DataProcessor where

import Data.Char (isDigit, isAlpha)
import Data.List (intercalate)

type ValidationRule = String -> Bool
type Transformation = String -> String

validateNumeric :: ValidationRule
validateNumeric = all isDigit

validateAlpha :: ValidationRule
validateAlpha = all isAlpha

transformToUpper :: Transformation
transformToUpper = map toUpper

transformPadRight :: Int -> Char -> Transformation
transformPadRight n c s = s ++ replicate (n - length s) c

processField :: ValidationRule -> Transformation -> String -> Maybe String
processField validate transform input
    | validate input = Just (transform input)
    | otherwise = Nothing

processRow :: [ValidationRule] -> [Transformation] -> [String] -> Maybe [String]
processRow validations transformations row
    | length validations == length transformations && length transformations == length row =
        sequence $ zipWith3 processField validations transformations row
    | otherwise = Nothing

formatCSVRow :: [String] -> String
formatCSVRow = intercalate ","

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

validateCSVData :: [[String]] -> [ValidationRule] -> [Transformation] -> Maybe [String]
validateCSVData rows validations transformations = do
    processedRows <- mapM (processRow validations transformations) rows
    return $ map formatCSVRow processedRows