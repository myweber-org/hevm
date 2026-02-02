module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = all (\x -> x >= -100 && x <= 100) xs

safeProcess :: [Int] -> Maybe [Int]
safeProcess xs
    | validateInput xs = Just (processData xs)
    | otherwise = Nothingmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [1, -2, 3, 0, 5, -8]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

import Data.List.Split (splitOn)
import Text.Read (readMaybe)

type Record = (String, Double, Double)

parseCSV :: String -> Maybe [Record]
parseCSV csvData = mapM parseLine (lines csvData)
  where
    parseLine line = case splitOn "," line of
      [name, val1, val2] -> do
        v1 <- readMaybe val1
        v2 <- readMaybe val2
        return (name, v1, v2)
      _ -> Nothing

calculateAverages :: [Record] -> (Double, Double)
calculateAverages records = (avg firsts, avg seconds)
  where
    firsts  = map (\(_, x, _) -> x) records
    seconds = map (\(_, _, y) -> y) records
    avg xs = sum xs / fromIntegral (length xs)

processData :: String -> Maybe (Double, Double)
processData input = do
  records <- parseCSV input
  return $ calculateAverages records
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (* 2)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    print result