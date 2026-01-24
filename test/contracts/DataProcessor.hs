
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)module DataProcessor where

import Data.List (transpose)
import Text.Read (readMaybe)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> Either String CSVData
parseCSV content = case lines content of
    [] -> Left "Empty file"
    rows -> let parsedRows = map (splitOn ',') rows
            in if allEqualLength parsedRows
                then Right parsedRows
                else Left "Rows have inconsistent column counts"
  where
    splitOn :: Char -> String -> Row
    splitOn delimiter = foldr splitHelper [[]]
      where
        splitHelper ch (current:rest)
            | ch == delimiter = []:current:rest
            | otherwise = (ch:current):rest

allEqualLength :: [Row] -> Bool
allEqualLength [] = True
allEqualLength (x:xs) = all ((== length x) . length) xs

validateNumericColumns :: CSVData -> [Int] -> Either String CSVData
validateNumericColumns dataRows columnIndices =
    case invalidColumns of
        [] -> Right dataRows
        _ -> Left $ "Non-numeric values found in columns: " ++ show invalidColumns
  where
    invalidColumns = filter (not . isColumnNumeric) columnIndices
    isColumnNumeric idx = all (isNumeric . (!! idx)) dataRows
    isNumeric str = case readMaybe str :: Maybe Double of
        Just _ -> True
        Nothing -> False

calculateColumnAverages :: CSVData -> [Int] -> Either String [Double]
calculateColumnAverages dataRows columnIndices
    | null dataRows = Left "No data rows"
    | any (>= length (head dataRows)) columnIndices = Left "Column index out of bounds"
    | otherwise = Right $ map calculateAverage columnIndices
  where
    calculateAverage idx = 
        let values = map (!! idx) dataRows
            numericValues = mapMaybe (readMaybe :: String -> Maybe Double) values
        in if null numericValues 
            then 0.0 
            else sum numericValues / fromIntegral (length numericValues)

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = case f x of
    Just y -> y : mapMaybe f xs
    Nothing -> mapMaybe f xs
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput xs
    | null xs = Nothing
    | any (< -100) xs = Nothing
    | any (> 100) xs = Nothing
    | otherwise = Just xs

safeDataProcessing :: [Int] -> Maybe [Int]
safeDataProcessing input = do
    validated <- validateInput input
    return $ processData validated
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
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

main :: IO ()
main = do
    let numbers = [1, -2, 3, -4, 5]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)
    putStrLn $ "First element: " ++ show (safeHead numbers)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-5, 3, 0, 8, -2, 10]
    let result = processNumbers numbers
    print result