module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = all (\x -> x >= -100 && x <= 100) xs

main :: IO ()
main = do
    let sampleData = [1, -2, 3, 0, 5, -8]
    if validateInput sampleData
        then print $ processData sampleData
        else putStrLn "Input validation failed"
module DataProcessor where

import Data.Time
import Text.CSV

filterByDateRange :: Day -> Day -> [Record] -> [Record]
filterByDateRange startDate endDate records =
  filter (\record -> case parseDate record of
            Just date -> date >= startDate && date <= endDate
            Nothing   -> False) records
  where
    parseDate :: Record -> Maybe Day
    parseDate record
      | length record >= 2 = parseTimeM True defaultTimeLocale "%Y-%m-%d" (head (tail record))
      | otherwise = Nothing

calculateDailyAverage :: [Record] -> [(Day, Double)]
calculateDailyAverage records =
  let grouped = foldr groupByDate [] records
  in map (\(date, values) -> (date, sum values / fromIntegral (length values))) grouped
  where
    groupByDate :: Record -> [(Day, [Double])] -> [(Day, [Double])]
    groupByDate record acc =
      case parseDate record of
        Just date ->
          case lookup date acc of
            Just existing -> (date, parseValue record : existing) : filter ((/= date) . fst) acc
            Nothing       -> (date, [parseValue record]) : acc
        Nothing -> acc
    
    parseValue :: Record -> Double
    parseValue record
      | length record >= 3 = read (record !! 2)
      | otherwise = 0.0