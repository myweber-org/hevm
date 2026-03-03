
module DataProcessor where

import Data.List (intercalate)
import Text.Read (readMaybe)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ',') (lines content)
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitHelper [""]
      where
        splitHelper char acc@(current:rest)
          | char == delimiter = "":acc
          | otherwise = (char:current):rest

safeReadDouble :: String -> Maybe Double
safeReadDouble = readMaybe

computeColumnStats :: CSVData -> Int -> Maybe (Double, Double, Double)
computeColumnStats rows colIndex
  | null rows = Nothing
  | otherwise = do
      let columnValues = mapMaybe (safeReadDouble . (!! colIndex)) rows
      if null columnValues
        then Nothing
        else let sumVal = sum columnValues
                 count = fromIntegral (length columnValues)
                 avg = sumVal / count
                 minVal = minimum columnValues
                 maxVal = maximum columnValues
             in Just (avg, minVal, maxVal)

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = case f x of
  Just y -> y : mapMaybe f xs
  Nothing -> mapMaybe f xs

formatStats :: (Double, Double, Double) -> String
formatStats (avg, minVal, maxVal) =
  "Average: " ++ show avg ++
  ", Min: " ++ show minVal ++
  ", Max: " ++ show maxVal

processCSVFile :: String -> Int -> String
processCSVFile content colIndex =
  case computeColumnStats (parseCSV content) colIndex of
    Just stats -> formatStats stats
    Nothing -> "Unable to compute statistics for column " ++ show colIndex

sampleCSV :: String
sampleCSV = intercalate "\n"
  [ "Name,Age,Salary"
  , "Alice,30,50000"
  , "Bob,25,45000"
  , "Charlie,35,60000"
  , "Diana,28,52000"
  ]