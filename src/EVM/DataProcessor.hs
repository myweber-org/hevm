module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isAlpha)

type CSVRow = [String]
type ValidationError = String

validateRow :: CSVRow -> Either ValidationError CSVRow
validateRow row
    | length row /= 3 = Left "Row must contain exactly 3 columns"
    | not (all validName [row !! 0]) = Left "First column must contain only letters"
    | not (all isDigit (row !! 1)) = Left "Second column must contain only digits"
    | not (validAge (row !! 2)) = Left "Third column must be a valid age (1-150)"
    | otherwise = Right row
  where
    validName = all isAlpha
    validAge ageStr = 
        case reads ageStr :: [(Int, String)] of
            [(age, "")] -> age >= 1 && age <= 150
            _ -> False

parseCSV :: String -> Either ValidationError [CSVRow]
parseCSV content = 
    let rows = map (splitOn ',') (lines content)
        validatedRows = map validateRow rows
    in case partitionEithers validatedRows of
        ([], validRows) -> Right validRows
        (errors, _) -> Left (intercalate "; " errors)

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr splitHelper [""]
  where
    splitHelper char acc
        | char == delimiter = "":acc
        | otherwise = (char:head acc):tail acc

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr select ([], [])
  where
    select (Left a) (as, bs) = (a:as, bs)
    select (Right b) (as, bs) = (as, b:bs)

processCSVData :: String -> IO ()
processCSVData filename = do
    content <- readFile filename
    case parseCSV content of
        Left err -> putStrLn $ "Validation error: " ++ err
        Right rows -> do
            putStrLn "Valid CSV data:"
            mapM_ (putStrLn . intercalate " | ") rows
            let total = length rows
            putStrLn $ "Total valid records: " ++ show totalmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

import Data.List (tails)

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | n > length xs = error "Window size exceeds list length"
    | otherwise = map avg $ filter (\w -> length w == n) $ tails xs
  where
    avg window = sum window / fromIntegral n

smoothData :: Int -> [Double] -> [Double]
smoothData windowSize = movingAverage windowSize

calculateTrend :: [Double] -> Double
calculateTrend values = (last values - head values) / fromIntegral (length values - 1)

processDataset :: Int -> [Double] -> (Double, [Double])
processDataset windowSize dataset =
    let smoothed = smoothData windowSize dataset
        trend = calculateTrend smoothed
    in (trend, smoothed)