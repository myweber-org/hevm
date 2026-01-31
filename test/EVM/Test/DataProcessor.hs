module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [1, -2, 3, 0, 5, -8]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

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

import Data.Time
import Data.Time.Format
import Data.List
import Text.Read (readMaybe)

type CSVRow = [String]

parseCSV :: String -> [CSVRow]
parseCSV = map (splitOn ',') . lines
  where splitOn _ [] = []
        splitOn delimiter str =
          let (token, rest) = break (== delimiter) str
          in token : case rest of
                      [] -> []
                      (_:xs) -> splitOn delimiter xs

filterByDateRange :: Day -> Day -> [CSVRow] -> [CSVRow]
filterByDateRange startDate endDate rows =
  filter (isWithinRange . parseDate . head) rows
  where
    parseDate dateStr = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" dateStr :: Day
    isWithinRange date = date >= startDate && date <= endDate

calculateDailyAverage :: [CSVRow] -> Maybe Double
calculateDailyAverage rows =
  if null validValues
    then Nothing
    else Just (sum validValues / fromIntegral (length validValues))
  where
    validValues = mapMaybe (readMaybe . (!!1)) rows

processData :: String -> Day -> Day -> Maybe Double
processData csvData start end =
  calculateDailyAverage $ filterByDateRange start end $ parseCSV csvData