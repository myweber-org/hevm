
module DataProcessor where

import Data.Time
import Text.CSV

filterCSVByDate :: Day -> Day -> CSV -> CSV
filterCSVByDate startDate endDate csv =
    let header = head csv
        records = tail csv
        filteredRecords = filter (isWithinDateRange startDate endDate) records
    in header : filteredRecords

isWithinDateRange :: Day -> Day -> Record -> Bool
isWithinDateRange start end record =
    case parseDate (record !! 0) of
        Just date -> date >= start && date <= end
        Nothing   -> False

parseDate :: String -> Maybe Day
parseDate str = parseTimeM True defaultTimeLocale "%Y-%m-%d" str