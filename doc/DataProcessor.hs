
module DataProcessor where

import Data.List (intercalate)
import Text.Read (readMaybe)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ',') (lines content)
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr f [[]]
      where
        f c (x:xs) | c == delimiter = []:x:xs
                   | otherwise = (c:x):xs

safeReadDouble :: String -> Maybe Double
safeReadDouble = readMaybe

columnStats :: CSVData -> Int -> Maybe (Double, Double, Double)
columnStats rows colIndex
  | null rows = Nothing
  | otherwise = case mapM (safeReadDouble . (!! colIndex)) rows of
      Just values -> let avg = sum values / fromIntegral (length values)
                         mx = maximum values
                         mn = minimum values
                     in Just (avg, mx, mn)
      Nothing -> Nothing

formatStats :: (Double, Double, Double) -> String
formatStats (avg, mx, mn) =
  intercalate ", " [ "Average: " ++ show avg
                   , "Maximum: " ++ show mx
                   , "Minimum: " ++ show mn
                   ]

processCSVFile :: String -> String
processCSVFile content =
  case parseCSV content of
    [] -> "No data found"
    rows@(header:_) ->
      let statsResults = map (\i -> (header !! i, columnStats rows i)) [0..length header - 1]
          formatted = map (\(colName, mStats) ->
            case mStats of
              Just stats -> colName ++ ": " ++ formatStats stats
              Nothing -> colName ++ ": Non-numeric data") statsResults
      in unlines formatted