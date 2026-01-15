
module DataProcessor where

import Data.Time
import Data.Time.Format
import Data.List
import Data.Maybe

type CSVRow = [String]
type CSVData = [CSVRow]

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

filterByDateRange :: CSVData -> String -> String -> CSVData
filterByDateRange rows startStr endStr = 
    let maybeStart = parseDate startStr
        maybeEnd = parseDate endStr
        header = take 1 rows
        dataRows = drop 1 rows
    in case (maybeStart, maybeEnd) of
        (Just startDay, Just endDay) -> 
            header ++ filter (isWithinRange startDay endDay) dataRows
        _ -> rows
    where
        isWithinRange startDay endDay row =
            case parseDate (head row) of
                Just day -> day >= startDay && day <= endDay
                Nothing -> False

calculateAverage :: CSVData -> String -> Maybe Double
calculateAverage rows columnName =
    case findIndex (== columnName) (head rows) of
        Just idx -> 
            let values = mapMaybe (safeRead . (!! idx)) (drop 1 rows)
            in if null values 
                then Nothing 
                else Just (sum values / fromIntegral (length values))
        Nothing -> Nothing
    where
        safeRead s = case reads s of
            [(x, "")] -> Just x
            _ -> Nothing

formatOutput :: CSVData -> String
formatOutput = unlines . map (intercalate ",")module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print result