module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [1, -2, 3, -4, 5]
    let result = processNumbers numbers
    print result
module DataProcessor where

import Data.List (foldl')
import Text.CSV (parseCSV, Record)

type SummaryStats = (Double, Double, Double, Double)

computeStats :: [Double] -> SummaryStats
computeStats [] = (0, 0, 0, 0)
computeStats xs = (minimum xs, maximum xs, mean, stdDev)
  where
    mean = sum xs / fromIntegral (length xs)
    variance = sum (map (\x -> (x - mean) ^ 2) xs) / fromIntegral (length xs)
    stdDev = sqrt variance

parseNumericCSV :: String -> Either String [Double]
parseNumericCSV csvData = do
  parsed <- parseCSV "input" csvData
  let records = concat parsed
  traverse parseDouble records
  where
    parseDouble :: String -> Either String Double
    parseDouble s = case reads s of
      [(val, "")] -> Right val
      _ -> Left $ "Invalid numeric value: " ++ s

processCSVData :: String -> Either String SummaryStats
processCSVData csvData = do
  numbers <- parseNumericCSV csvData
  if null numbers
    then Left "No valid numeric data found"
    else Right $ computeStats numbers

main :: IO ()
main = do
  let testData = "1.5,2.3,3.7,4.1,5.8\n6.2,7.4,8.0,9.9,10.2"
  case processCSVData testData of
    Left err -> putStrLn $ "Error: " ++ err
    Right (minVal, maxVal, avg, sd) -> do
      putStrLn $ "Minimum: " ++ show minVal
      putStrLn $ "Maximum: " ++ show maxVal
      putStrLn $ "Mean: " ++ show avg
      putStrLn $ "Standard Deviation: " ++ show sd