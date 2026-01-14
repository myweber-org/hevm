module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput xs
    | null xs = Nothing
    | any (< -1000) xs = Nothing
    | any (> 1000) xs = Nothing
    | otherwise = Just xs

safeProcess :: [Int] -> Maybe [Int]
safeProcess xs = do
    validated <- validateInput xs
    return $ processData validated
module DataProcessor where

import Data.Time
import Text.CSV

filterCSVByDate :: String -> Day -> Day -> Either String [Record]
filterCSVByDate csvContent startDate endDate =
    case parseCSV "input" csvContent of
        Left err -> Left $ "CSV parse error: " ++ err
        Right csv -> Right $ filter (isInDateRange startDate endDate) csv

isInDateRange :: Day -> Day -> Record -> Bool
isInDateRange start end record =
    case record of
        (dateStr:_) -> 
            case parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr of
                Just date -> date >= start && date <= end
                Nothing -> False
        _ -> False

processData :: [Record] -> [Double]
processData records = 
    map (maybe 0 id . parseValue . head . drop 1) records
    where parseValue str = case reads str of
                            [(val, "")] -> Just val
                            _ -> Nothing