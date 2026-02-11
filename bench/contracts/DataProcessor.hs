
module DataProcessor where

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

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)
module DataProcessor where

import Data.Time
import Data.Time.Format
import Data.List
import Data.Maybe
import Text.Read

type Record = (Day, String, Double)

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

parseRecord :: String -> Maybe Record
parseRecord line = case words line of
    [dateStr, label, valueStr] -> do
        date <- parseDate dateStr
        value <- readMaybe valueStr
        return (date, label, value)
    _ -> Nothing

filterByDateRange :: Day -> Day -> [Record] -> [Record]
filterByDateRange start end = filter (\(d, _, _) -> d >= start && d <= end)

loadRecords :: FilePath -> IO [Record]
loadRecords path = do
    content <- readFile path
    return $ catMaybes $ map parseRecord (lines content)

summarizeRecords :: [Record] -> [(String, Double)]
summarizeRecords records =
    let groups = groupBy (\(_, l1, _) (_, l2, _) -> l1 == l2) $ sortOn (\(_, l, _) -> l) records
    in map (\g -> let (_, label, _) = head g in (label, sum $ map (\(_, _, v) -> v) g)) groups

processDataFile :: FilePath -> Day -> Day -> IO [(String, Double)]
processDataFile path start end = do
    records <- loadRecords path
    let filtered = filterByDateRange start end records
    return $ summarizeRecords filtered
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers
module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isAlpha, isSpace)

type ValidationRule = String -> Bool
type Transformation = String -> String

validateNonEmpty :: ValidationRule
validateNonEmpty = not . null

validateNumeric :: ValidationRule
validateNumeric = all (\c -> isDigit c || c == '.')

validateAlpha :: ValidationRule
validateAlpha = all isAlpha

trimWhitespace :: Transformation
trimWhitespace = reverse . dropWhile isSpace . reverse . dropWhile isSpace

normalizeCase :: Transformation
normalizeCase = map toLower

validateRow :: [ValidationRule] -> [String] -> [Bool]
validateRow rules row = zipWith ($) rules row

transformRow :: [Transformation] -> [String] -> [String]
transformRow transforms row = zipWith ($) transforms row

processCSVRow :: [ValidationRule] -> [Transformation] -> [String] -> Maybe [String]
processCSVRow validations transforms row
    | all id validationResults = Just $ transformRow transforms row
    | otherwise = Nothing
    where validationResults = validateRow validations row

formatOutput :: [String] -> String
formatOutput = intercalate ","

safeReadDouble :: String -> Maybe Double
safeReadDouble str = case reads str of
    [(num, "")] -> Just num
    _ -> Nothing

sanitizeInput :: String -> String
sanitizeInput = filter (\c -> isAlpha c || isDigit c || c `elem` " .,-")

data ProcessingResult = ValidRow [String] | InvalidRow [String] | EmptyRow

classifyRow :: [String] -> ProcessingResult
classifyRow [] = EmptyRow
classifyRow row
    | any null row = InvalidRow row
    | otherwise = ValidRow row