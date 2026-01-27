
module DataProcessor where

import Data.List (foldl')
import Text.Read (readMaybe)

data Row = Row { name :: String, value :: Double } deriving (Show, Eq)

parseRow :: String -> Maybe Row
parseRow line = case words line of
    [n, v] -> case readMaybe v of
        Just d  -> Just (Row n d)
        Nothing -> Nothing
    _ -> Nothing

parseCSV :: String -> [Row]
parseCSV = mapMaybe parseRow . lines
  where mapMaybe f = foldr (\x acc -> case f x of Just y -> y : acc; Nothing -> acc) []

computeStats :: [Row] -> (Double, Double, Double)
computeStats rows = (avg, minVal, maxVal)
  where
    values = map value rows
    sumVal = foldl' (+) 0 values
    count = fromIntegral (length values)
    avg = if count > 0 then sumVal / count else 0
    minVal = foldl' min (1/0) values
    maxVal = foldl' max (-1/0) values

processData :: String -> String
processData input = case rows of
    [] -> "No valid data found"
    _  -> "Average: " ++ show avg ++ "\n" ++
          "Minimum: " ++ show minVal ++ "\n" ++
          "Maximum: " ++ show maxVal
  where
    rows = parseCSV input
    (avg, minVal, maxVal) = computeStats rows