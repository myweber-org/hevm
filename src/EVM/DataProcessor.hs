
module DataProcessor where

import Data.List (intercalate)
import Text.Read (readMaybe)

type Row = [String]
type Table = [Row]

parseCSV :: String -> Table
parseCSV content = map (splitOn ',') (lines content)
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitHelper [""]
      where
        splitHelper :: Char -> [String] -> [String]
        splitHelper ch (current:rest)
          | ch == delimiter = "":current:rest
          | otherwise = (ch:current):rest

filterRows :: (Row -> Bool) -> Table -> Table
filterRows predicate = filter predicate

numericColumnGreaterThan :: Int -> Double -> Row -> Bool
numericColumnGreaterThan columnIndex thresholdValue row =
  case getColumn columnIndex row of
    Just str -> case readMaybe str of
      Just val -> val > thresholdValue
      Nothing -> False
    Nothing -> False
  where
    getColumn :: Int -> Row -> Maybe String
    getColumn idx r
      | idx >= 0 && idx < length r = Just (r !! idx)
      | otherwise = Nothing

tableToCSV :: Table -> String
tableToCSV = intercalate "\n" . map (intercalate ",")

processData :: String -> (Row -> Bool) -> String
processData csvContent predicate =
  tableToCSV $ filterRows predicate $ parseCSV csvContent