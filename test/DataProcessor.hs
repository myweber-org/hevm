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

import Data.Time
import Text.CSV

filterCSVByDate :: Day -> Day -> CSV -> CSV
filterCSVByDate startDate endDate (header:rows) =
    header : filter (isWithinDateRange startDate endDate) rows
  where
    isWithinDateRange start end row =
      case parseDate (head row) of
        Just date -> date >= start && date <= end
        Nothing   -> False

    parseDate :: String -> Maybe Day
    parseDate str = parseTimeM True defaultTimeLocale "%Y-%m-%d" str

processData :: CSV -> [(String, Double)]
processData csv = map aggregateRow (tail csv)
  where
    aggregateRow row = (row !! 1, sum (map read (drop 2 row)))