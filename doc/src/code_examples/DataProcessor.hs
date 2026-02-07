module DataProcessor where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Record = (String, Double)

parseCSV :: String -> [Record]
parseCSV content = mapMaybe parseLine (lines content)
  where
    parseLine line = case splitOn "," line of
      [name, valueStr] -> case reads valueStr of
        [(value, "")] -> Just (name, value)
        _ -> Nothing
      _ -> Nothing

calculateAverage :: [Record] -> Double
calculateAverage records =
  if null records
    then 0.0
    else total / fromIntegral (length records)
  where
    total = sum (map snd records)

filterAboveAverage :: [Record] -> [Record]
filterAboveAverage records =
  let avg = calculateAverage records
   in filter (\(_, value) -> value > avg) records

processData :: String -> [Record]
processData = filterAboveAverage . parseCSVmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (* 2)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show result