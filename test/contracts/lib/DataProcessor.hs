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

import Data.Char (isDigit, toUpper)

-- Validate if a string contains only digits
validateDigits :: String -> Bool
validateDigits = all isDigit

-- Transform a string to uppercase
transformToUpper :: String -> String
transformToUpper = map toUpper

-- Process data: validate digits and transform to uppercase if valid
processData :: String -> Maybe String
processData input
    | validateDigits input = Just (transformToUpper input)
    | otherwise = Nothing

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
    let testData1 = "12345"
        testData2 = "12a45"
    print $ processData testData1
    print $ processData testData2module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, 0, 5, -8]
    let result = processData input
    print result