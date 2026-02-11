
module DataProcessor where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Record = (String, Double)

parseCSV :: String -> [Record]
parseCSV csvData = mapMaybe parseLine (lines csvData)
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

processCSVData :: String -> [Record]
processCSVData = filterAboveAverage . parseCSVmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData = all (> 0)

main :: IO ()
main = do
    let sampleData = [1, -2, 3, 0, 5, -8]
    putStrLn $ "Original data: " ++ show sampleData
    putStrLn $ "Processed data: " ++ show (processData sampleData)
    putStrLn $ "All positive? " ++ show (validateData sampleData)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessed :: (Int -> Int) -> [Int] -> Int
sumProcessed processor = sum . map processor

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn "Original list:"
    print numbers
    
    putStrLn "\nSquares of even numbers:"
    let squares = processEvenSquares numbers
    print squares
    
    putStrLn "\nSum of squares:"
    print $ sumProcessed (\x -> x * x) squares
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, 0, 5, -7]
    let result = processData input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result