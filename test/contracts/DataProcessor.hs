
module DataProcessor where

import Data.Time
import Text.CSV

filterCSVByDate :: Day -> Day -> CSV -> Either String CSV
filterCSVByDate startDate endDate csv = do
    let header = head csv
    records <- mapM parseRecord (tail csv)
    let filtered = filter (\(_, dateStr, _) -> 
            case parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr of
                Just date -> date >= startDate && date <= endDate
                Nothing -> False) records
    return $ header : map (\(id, date, value) -> [id, date, value]) filtered
  where
    parseRecord [id, date, value] = Right (id, date, value)
    parseRecord _ = Left "Invalid record format"