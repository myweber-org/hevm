module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | length xs < n = []
    | otherwise = avg : movingAverage n (tail xs)
    where
        window = take n xs
        avg = sum window / fromIntegral n

smoothData :: Fractional a => Int -> [a] -> [a]
smoothData windowSize dataPoints =
    let padSize = windowSize `div` 2
        padded = replicate padSize (head dataPoints) ++ dataPoints ++ replicate padSize (last dataPoints)
    in movingAverage windowSize padded

calculateTrend :: (Fractional a, Ord a) => [a] -> String
calculateTrend values
    | null values = "No data"
    | last values > head values = "Increasing"
    | last values < head values = "Decreasing"
    | otherwise = "Stable"

processDataset :: Fractional a => Int -> [a] -> ([a], String)
processDataset windowSize dataset =
    let smoothed = smoothData windowSize dataset
        trend = calculateTrend smoothed
    in (smoothed, trend)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers
module DataProcessor where

import Data.Char (isDigit, isAlpha, toUpper)
import Data.List (intercalate)

-- Validate if a string contains only digits
validateNumeric :: String -> Bool
validateNumeric = all isDigit

-- Validate if a string contains only alphabetic characters
validateAlpha :: String -> Bool
validateAlpha = all isAlpha

-- Convert string to uppercase
toUppercase :: String -> String
toUppercase = map toUpper

-- Normalize phone number by removing non-digit characters
normalizePhone :: String -> String
normalizePhone = filter isDigit

-- Format name as "Last, First"
formatName :: String -> String -> String
formatName first last = last ++ ", " ++ first

-- Process a list of strings with validation and transformation
processData :: [String] -> [String]
processData = map processItem
  where
    processItem str
      | validateNumeric str = "NUMERIC: " ++ normalizePhone str
      | validateAlpha str = "ALPHA: " ++ toUppercase str
      | otherwise = "MIXED: " ++ str

-- Combine multiple strings with a separator
combineWithSeparator :: String -> [String] -> String
combineWithSeparator sep = intercalate sep . filter (not . null)

-- Safe head function with default value
safeHead :: a -> [a] -> a
safeHead def [] = def
safeHead _ (x:_) = x

-- Calculate average of a list of numbers
average :: [Double] -> Double
average [] = 0.0
average xs = sum xs / fromIntegral (length xs)

-- Main processing pipeline example
processPipeline :: [String] -> String
processPipeline input =
  let validated = filter (not . null) input
      processed = processData validated
      result = combineWithSeparator " | " processed
  in "RESULT: " ++ result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [1, -2, 3, -4, 5]
    let result = processNumbers numbers
    print result
module DataProcessor where

import Data.List (foldl')
import Text.Read (readMaybe)

type Row = [String]
type CSVData = [Row]

data SummaryStats = SummaryStats
    { count :: Int
    , sum   :: Double
    , mean  :: Double
    , min   :: Maybe Double
    , max   :: Maybe Double
    } deriving (Show, Eq)

parseCSV :: String -> CSVData
parseCSV = map (splitOn ',') . lines
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitHelper [""]
      where
        splitHelper char acc@(current:rest)
            | char == delimiter = "":acc
            | otherwise = (char:current):rest

computeNumericStats :: CSVData -> Int -> SummaryStats
computeNumericStats rows columnIndex
    | null numericValues = emptyStats
    | otherwise = SummaryStats
        { count = length numericValues
        , sum   = total
        , mean  = total / fromIntegral (length numericValues)
        , min   = Just minimumVal
        , max   = Just maximumVal
        }
  where
    numericValues = [v | row <- rows, 
                        length row > columnIndex,
                        let cell = row !! columnIndex,
                        Just v <- [readMaybe cell]]
    
    (total, minimumVal, maximumVal) = foldl' 
        (\(s, mn, mx) val -> (s + val, min mn val, max mx val))
        (0, head numericValues, head numericValues)
        (tail numericValues)
    
    emptyStats = SummaryStats 0 0 0 Nothing Nothing

validateColumn :: CSVData -> Int -> Bool
validateColumn rows colIndex = all (\row -> length row > colIndex) rows

processCSVFile :: String -> Int -> IO (Maybe SummaryStats)
processCSVFile filePath columnIndex = do
    content <- readFile filePath
    let parsedData = parseCSV content
    if null parsedData || not (validateColumn parsedData columnIndex)
        then return Nothing
        else return $ Just $ computeNumericStats parsedData columnIndex

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData xs = all (> 0) xs && length xs > 3