
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

processData :: CSV -> (Double, Double)
processData (header:rows) =
    let values = map (read . (!!1)) rows
        avg = sum values / fromIntegral (length values)
        variance = sum (map (\x -> (x - avg) ** 2) values) / fromIntegral (length values)
    in (avg, variance)