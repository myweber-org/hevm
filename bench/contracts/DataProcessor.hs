module DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter evenmodule DataProcessor where

import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)
import Text.Read (readMaybe)

data ValidationResult a = Valid a | Invalid String deriving (Show, Eq)

validateNonEmpty :: String -> ValidationResult String
validateNonEmpty "" = Invalid "Field cannot be empty"
validateNonEmpty s  = Valid s

validateNumeric :: String -> ValidationResult Int
validateNumeric s
    | all isDigit s = case readMaybe s of
        Just n  -> Valid n
        Nothing -> Invalid "Invalid numeric format"
    | otherwise = Invalid "Contains non-digit characters"

validateRange :: Int -> Int -> Int -> ValidationResult Int
validateRange minVal maxVal val
    | val >= minVal && val <= maxVal = Valid val
    | otherwise = Invalid $ "Value must be between " ++ show minVal ++ " and " ++ show maxVal

transformToUpper :: String -> String
transformToUpper = map toUpper

transformToLower :: String -> String
transformToLower = map toLower

safeParseInt :: String -> Maybe Int
safeParseInt = readMaybe

safeParseDouble :: String -> Maybe Double
safeParseDouble = readMaybe

processData :: [String] -> [ValidationResult Int]
processData = map (validateNumeric >=> validateRange 1 100)

formatResults :: [ValidationResult a] -> String
formatResults results = intercalate "\n" $ map formatResult results
  where
    formatResult (Valid val) = "✓ Valid: " ++ show val
    formatResult (Invalid err) = "✗ Invalid: " ++ err

filterValid :: [ValidationResult a] -> [a]
filterValid results = [x | Valid x <- results]

filterInvalid :: [ValidationResult a] -> [String]
filterInvalid results = [err | Invalid err <- results]

validateEmail :: String -> ValidationResult String
validateEmail email
    | '@' `elem` email && '.' `elem` (dropWhile (/= '@') email) = Valid email
    | otherwise = Invalid "Invalid email format"

type Transformer a b = a -> ValidationResult b

composeValidators :: Transformer a b -> Transformer b c -> Transformer a c
composeValidators f g x = case f x of
    Valid y -> g y
    Invalid err -> Invalid err

main :: IO ()
main = do
    let testData = ["42", "hello", "150", "75", ""]
    let processed = processData testData
    
    putStrLn "Validation Results:"
    putStrLn $ formatResults processed
    
    putStrLn "\nValid values:"
    print $ filterValid processed
    
    putStrLn "\nInvalid errors:"
    print $ filterInvalid processed
    
    putStrLn "\nEmail validation examples:"
    print $ validateEmail "test@example.com"
    print $ validateEmail "invalid-email"module DataProcessor where

import Data.List (tails)

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | n > length xs = error "Window size exceeds list length"
    | otherwise = map avg $ filter (\window -> length window == n) $ tails xs
  where
    avg window = sum window / fromIntegral n

smoothData :: Fractional a => Int -> [a] -> [a]
smoothData windowSize dataPoints =
    movingAverage windowSize dataPoints

calculateTrend :: Fractional a => [a] -> Maybe a
calculateTrend [] = Nothing
calculateTrend [_] = Nothing
calculateTrend values =
    let n = fromIntegral $ length values
        xSum = sum [0..n-1]
        ySum = sum values
        xySum = sum $ zipWith (*) values [0..]
        x2Sum = sum $ map (^2) [0..n-1]
        slope = (n * xySum - xSum * ySum) / (n * x2Sum - xSum * xSum)
    in Just slopemodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbers