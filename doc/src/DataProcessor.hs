module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

import Data.Char (toUpper)

data ValidationError = InvalidLength | InvalidFormat | EmptyInput
    deriving (Show, Eq)

validateInput :: String -> Either ValidationError String
validateInput "" = Left EmptyInput
validateInput s
    | length s < 3 = Left InvalidLength
    | not (all isAlpha s) = Left InvalidFormat
    | otherwise = Right s
    where
        isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

transformData :: String -> String
transformData = map toUpper

processData :: String -> Either ValidationError String
processData input = do
    validated <- validateInput input
    return $ transformData validated

safeProcess :: String -> String
safeProcess input = case processData input of
    Left err -> "Error: " ++ show err
    Right result -> "Processed: " ++ resultmodule DataProcessor where

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
tails xs@(_:ys) = xs : tails ys
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

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

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)