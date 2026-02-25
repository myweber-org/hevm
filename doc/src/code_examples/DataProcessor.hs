
module DataProcessor where

import Data.List (transpose)
import Text.Read (readMaybe)

parseCSV :: String -> [[String]]
parseCSV = map (splitOn ',') . lines
  where splitOn delimiter = foldr f [[]]
          where f c (x:xs) | c == delimiter = []:x:xs
                           | otherwise = (c:x):xs

safeReadDouble :: String -> Maybe Double
safeReadDouble = readMaybe

calculateColumnAverages :: [[String]] -> [Maybe Double]
calculateColumnAverages rows = map columnAverage transposed
  where transposed = transpose rows
        columnAverage col = case mapM safeReadDouble col of
          Just nums -> Just (sum nums / fromIntegral (length nums))
          Nothing -> Nothing

processCSVData :: String -> [Maybe Double]
processCSVData csv = calculateColumnAverages (parseCSV csv)

main :: IO ()
main = do
  let sampleData = "Name,Age,Score\nAlice,25,85.5\nBob,30,92.0\nCharlie,22,78.5"
  print $ processCSVData sampleData