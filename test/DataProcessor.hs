
module DataProcessor where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)

-- | Validates if a string contains only digits
validateDigits :: String -> Bool
validateDigits = all isDigit

-- | Safely parses an integer from string, returns Nothing on failure
safeParseInt :: String -> Maybe Int
safeParseInt str
    | validateDigits str = Just (read str)
    | otherwise = Nothing

-- | Normalizes phone number by removing spaces and dashes
normalizePhone :: String -> String
normalizePhone = filter (\c -> isDigit c || c == '+')

-- | Validates email format (basic check)
validateEmail :: String -> Bool
validateEmail email =
    let parts = split '@' email
    in length parts == 2 && 
       not (null (head parts)) && 
       '.' `elem` (last parts)
  where
    split delimiter = foldr (\c acc -> 
        if c == delimiter 
        then []:acc 
        else (c:head acc):tail acc) [[]]

-- | Processes a list of raw strings into validated integers
processNumbers :: [String] -> [Int]
processNumbers = catMaybes . map safeParseInt

-- | Formats a list of integers as a comma-separated string
formatNumbers :: [Int] -> String
formatNumbers = intercalate ", " . map show

-- | Main processing pipeline example
processData :: [String] -> String
processData inputs = 
    let validated = processNumbers inputs
        total = sum validated
        count = length validated
    in "Processed " ++ show count ++ 
       " numbers totaling " ++ show total

-- | Type-safe configuration parser
data Config = Config 
    { maxRetries :: Int
    , timeout :: Int
    , enabled :: Bool
    } deriving (Show, Eq)

parseConfig :: [(String, String)] -> Config
parseConfig pairs = Config
    { maxRetries = fromMaybe 3 (lookup "maxRetries" pairs >>= safeParseInt)
    , timeout = fromMaybe 30 (lookup "timeout" pairs >>= safeParseInt)
    , enabled = fromMaybe True (fmap read (lookup "enabled" pairs))
    }module DataProcessor where

import Data.List.Split (splitOn)

parseCSV :: String -> [[Double]]
parseCSV csvData = map (map read . splitOn ",") (lines csvData)

calculateAverages :: [[Double]] -> [Double]
calculateAverages rows = 
    if null rows then []
    else map (\col -> sum col / fromIntegral (length col)) (transpose rows)
  where
    transpose :: [[Double]] -> [[Double]]
    transpose [] = []
    transpose ([]:_) = []
    transpose xss = map head xss : transpose (map tail xss)

processCSVData :: String -> [Double]
processCSVData csvContent = 
    let parsedData = parseCSV csvContent
    in calculateAverages parsedData
module DataProcessor where

import Data.Time
import Text.CSV

filterCSVByDate :: String -> Day -> Day -> Either String [Record]
filterCSVByDate csvContent startDate endDate = do
    csv <- parseCSV "input" csvContent
    let filtered = filter (isWithinDateRange startDate endDate) csv
    return filtered

isWithinDateRange :: Day -> Day -> Record -> Bool
isWithinDateRange start end record =
    case record of
        (dateStr:_) -> 
            case parseDate dateStr of
                Just date -> date >= start && date <= end
                Nothing   -> False
        _ -> False

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"module DataProcessor where

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

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessed :: (Int -> Int) -> [Int] -> Int
sumProcessed f = foldr ((+) . f) 0

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original: " ++ show numbers
    putStrLn $ "Even squares: " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of even squares: " ++ show (sumProcessed (\x -> if even x then x*x else 0) numbers)