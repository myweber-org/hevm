module DataProcessor where

import Data.List (tails)

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | n > length xs = error "Window size exceeds list length"
    | otherwise = map average $ filter (\w -> length w == n) $ tails xs
  where
    average ws = sum ws / fromIntegral n

smoothData :: Int -> [Double] -> [Double]
smoothData windowSize dataPoints = 
    movingAverage windowSize dataPoints ++ 
    replicate (windowSize - 1) (last dataPoints)

validateData :: [Double] -> Maybe [Double]
validateData [] = Nothing
validateData xs
    | any isNaN xs = Nothing
    | any isInfinite xs = Nothing
    | otherwise = Just xs

processDataStream :: Int -> [Double] -> Maybe [Double]
processDataStream windowSize rawData = do
    validData <- validateData rawData
    return $ smoothData windowSize validDatamodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processDatamodule DataProcessor where

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
module DataProcessor where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)

-- | Safely parse an integer from a string, returning Nothing on failure
safeParseInt :: String -> Maybe Int
safeParseInt s = case reads s of
    [(n, "")] -> Just n
    _         -> Nothing

-- | Validate that a string contains only digits
validateDigits :: String -> Bool
validateDigits = all isDigit

-- | Trim leading and trailing whitespace
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

-- | Parse a comma-separated list of integers
parseIntList :: String -> Maybe [Int]
parseIntList input = 
    let trimmed = trim input
        parts = splitOn ',' trimmed
        parsed = map safeParseInt parts
    in if all isJust parsed
        then Just (catMaybes parsed)
        else Nothing
    where
        splitOn :: Char -> String -> [String]
        splitOn delimiter = foldr splitHelper [""]
            where
                splitHelper char acc@(current:rest)
                    | char == delimiter = "":acc
                    | otherwise = (char:current):rest

        isJust :: Maybe a -> Bool
        isJust (Just _) = True
        isJust Nothing  = False

-- | Transform a list of integers by applying a function and filtering
transformData :: (Int -> Int) -> (Int -> Bool) -> [Int] -> [Int]
transformData f predicate = map f . filter predicate

-- | Calculate statistics from a list of integers
data Stats = Stats
    { sumTotal :: Int
    , average  :: Double
    , minVal   :: Int
    , maxVal   :: Int
    } deriving (Show, Eq)

calculateStats :: [Int] -> Maybe Stats
calculateStats [] = Nothing
calculateStats xs = Just Stats
    { sumTotal = sum xs
    , average  = fromIntegral (sum xs) / fromIntegral (length xs)
    , minVal   = minimum xs
    , maxVal   = maxVal
    }
    where maxVal = maximum xs

-- | Format a list of integers as a string with custom separator
formatList :: String -> [Int] -> String
formatList separator = intercalate separator . map show

-- | Main processing pipeline
processData :: String -> Either String Stats
processData input = case parseIntList input of
    Nothing -> Left "Invalid input format"
    Just numbers -> case calculateStats numbers of
        Nothing -> Left "Empty data after parsing"
        Just stats -> Right statsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print result
module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows :: Int -> [a] -> [[a]]
    windows m = takeWhile ((== m) . length) . map (take m) . tails
    
    average :: Fractional a => [a] -> a
    average ys = sum ys / fromIntegral (length ys)

-- Helper function from Data.List
tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_:ys) = xs : tails ysmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processEvenSquares

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Even squares: " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of even squares: " ++ show (sumProcessed numbers)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData = all (> 0) . processData
module DataProcessor where

import Data.Time
import Text.CSV

filterByDateRange :: Day -> Day -> [Record] -> [Record]
filterByDateRange startDate endDate records = 
    filter (isWithinRange . parseDate . head) records
  where
    parseDate dateStr = 
        case parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr of
            Just day -> day
            Nothing -> error $ "Invalid date format: " ++ dateStr
    
    isWithinRange day = day >= startDate && day <= endDate

processCSVData :: String -> Day -> Day -> Either String [Record]
processCSVData csvContent start end = do
    csv <- parseCSV "input" csvContent
    case csv of
        Left err -> Left $ "CSV parse error: " ++ err
        Right records -> 
            if null records 
            then Left "Empty CSV data"
            else Right $ filterByDateRange start end (tail records)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed numbers: " ++ show (sumProcessed numbers)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform isEven square
    where
        isEven x = x `mod` 2 == 0
        square x = x * x

calculateStats :: [Int] -> (Int, Int, Double)
calculateStats xs = (minimum xs, maximum xs, average xs)
    where
        average [] = 0.0
        average ys = fromIntegral (sum ys) / fromIntegral (length ys)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just xmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processNumbers input
    print result
module DataProcessor where

import Data.Time
import Data.Time.Format
import Data.List
import Data.Maybe
import Text.CSV

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

filterByDateRange :: Day -> Day -> [(Day, String, Double)] -> [(Day, String, Double)]
filterByDateRange start end = filter (\(date, _, _) -> date >= start && date <= end)

parseCSVToRecords :: String -> Either String [(Day, String, Double)]
parseCSVToRecords csvData = do
    parsed <- parseCSV "data" csvData
    let records = tail parsed
    traverse parseRow records
  where
    parseRow [dateStr, name, valueStr] = do
        date <- maybeToEither ("Invalid date: " ++ dateStr) (parseDate dateStr)
        value <- maybeToEither ("Invalid value: " ++ valueStr) (readMaybe valueStr)
        return (date, name, value)
    parseRow row = Left $ "Invalid row format: " ++ show row

    maybeToEither err Nothing = Left err
    maybeToEither _ (Just x) = Right x

    readMaybe :: String -> Maybe Double
    readMaybe s = case reads s of
        [(x, "")] -> Just x
        _ -> Nothing

summarizeByCategory :: [(Day, String, Double)] -> [(String, Double)]
summarizeByCategory = map combine . groupBy sameCategory . sortBy categoryOrder
  where
    sameCategory (_, cat1, _) (_, cat2, _) = cat1 == cat2
    categoryOrder (_, cat1, _) (_, cat2, _) = compare cat1 cat2
    combine group = (category, sum values)
      where
        category = let (_, cat, _) = head group in cat
        values = map (\(_, _, val) -> val) group

processCSVData :: Day -> Day -> String -> Either String [(String, Double)]
processCSVData start end csvData = do
    records <- parseCSVToRecords csvData
    let filtered = filterByDateRange start end records
    return $ summarizeByCategory filteredmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (*2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData xs = all (> 0) xs && length xs > 3

combineProcessors :: [Int] -> [Int]
combineProcessors xs
    | validateData xs = processData xs
    | otherwise = []module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -1000) xs then Just xs else Nothingmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print result