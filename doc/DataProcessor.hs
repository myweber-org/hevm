module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows m ys = take (length ys - m + 1) $ zipWith (++) (tails ys) (repeat [])
    average zs = sum zs / fromIntegral (length zs)module DataProcessor where

processNumbers :: [Int] -> [Int]
processNumbers = map (*2) . filter (>0)module DataProcessor where

import Data.List (foldl')
import Text.Read (readMaybe)

data StatRecord = StatRecord
    { recordId :: Int
    , value    :: Double
    } deriving (Show, Eq)

parseCSVLine :: String -> Maybe StatRecord
parseCSVLine line = case words line of
    [idStr, valStr] -> do
        idVal <- readMaybe idStr
        dVal  <- readMaybe valStr
        return $ StatRecord idVal dVal
    _ -> Nothing

parseCSVData :: String -> [StatRecord]
parseCSVData = mapMaybe parseCSVLine . lines
  where
    mapMaybe f = foldr (\x acc -> case f x of
        Just val -> val : acc
        Nothing  -> acc) []

computeStats :: [StatRecord] -> (Double, Double, Double)
computeStats records =
    let values = map value records
        sumVal = foldl' (+) 0 values
        len = fromIntegral $ length values
        mean = sumVal / len
        variance = foldl' (\acc v -> acc + (v - mean) ** 2) 0 values / len
        stdDev = sqrt variance
    in (mean, variance, stdDev)

processCSV :: String -> Maybe (Double, Double, Double)
processCSV input =
    let records = parseCSVData input
    in if null records
        then Nothing
        else Just $ computeStats records
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processDatamodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (* 2)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show resultmodule DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter even
module DataProcessor where

import Data.Time
import Data.Time.Format
import Data.List
import Data.Char
import System.Locale

type CSVRow = [String]
type DateRange = (Day, Day)

parseDate :: String -> Maybe Day
parseDate str = parseTimeM True defaultTimeLocale "%Y-%m-%d" str

parseCSVRow :: String -> CSVRow
parseCSVRow = splitByComma . trim
  where
    splitByComma = unfoldr (\s -> case break (== ',') s of
                                 (x, ',' : rest) -> Just (x, rest)
                                 (x, "") -> Just (x, "")
                                 _ -> Nothing)
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

filterByDateRange :: DateRange -> [(Day, CSVRow)] -> [(Day, CSVRow)]
filterByDateRange (start, end) = filter (\(day, _) -> day >= start && day <= end)

processCSVData :: String -> DateRange -> Maybe [(Day, CSVRow)]
processCSVData csvContent dateRange = do
    let rows = map parseCSVRow $ lines csvContent
    datedRows <- mapM extractDate rows
    return $ filterByDateRange dateRange datedRows
  where
    extractDate (dateStr : rest) = do
        day <- parseDate dateStr
        return (day, rest)
    extractDate _ = Nothing

formatOutput :: [(Day, CSVRow)] -> String
formatOutput = unlines . map (\(day, row) -> formatTime defaultTimeLocale "%Y-%m-%d" day ++ "," ++ intercalate "," row)

main :: IO ()
main = do
    let sampleData = "2023-01-15,ProductA,100\n2023-02-20,ProductB,150\n2023-03-10,ProductC,200"
    let range = (fromGregorian 2023 1 1, fromGregorian 2023 2 28)
    case processCSVData sampleData range of
        Just filtered -> putStrLn $ formatOutput filtered
        Nothing -> putStrLn "Error processing CSV data"module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData xs = all (> 0) xs && length xs > 0

main :: IO ()
main = do
    let sampleData = [1, -2, 3, 0, 5, -8]
    let processed = processData sampleData
    putStrLn $ "Original data: " ++ show sampleData
    putStrLn $ "Processed data: " ++ show processed
    putStrLn $ "Validation result: " ++ show (validateData processed)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData = all (> 0)

main :: IO ()
main = do
    let sampleData = [1, -2, 3, -4, 5]
    putStrLn $ "Original data: " ++ show sampleData
    putStrLn $ "Processed data: " ++ show (processData sampleData)
    putStrLn $ "Data valid: " ++ show (validateData sampleData)