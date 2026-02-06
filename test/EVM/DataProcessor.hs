module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = not (null xs) && all (>= -100) xs && all (<= 100) xs

safeProcess :: [Int] -> Maybe [Int]
safeProcess xs
  | validateInput xs = Just (processData xs)
  | otherwise = Nothing

exampleUsage :: IO ()
exampleUsage = do
  let input = [-5, 2, 0, 8, -1, 10]
  case safeProcess input of
    Just result -> putStrLn $ "Processed result: " ++ show result
    Nothing -> putStrLn "Invalid input detected"module DataProcessor where

import Data.List.Split (splitOn)

type Record = (String, Int, Double)

parseCSV :: String -> [Record]
parseCSV content = map parseLine (lines content)
  where
    parseLine line = case splitOn "," line of
      [name, ageStr, scoreStr] -> (name, read ageStr, read scoreStr)
      _ -> error "Invalid CSV format"

calculateAverageScore :: [Record] -> Double
calculateAverageScore records = 
  if null records 
    then 0.0
    else totalScore / fromIntegral (length records)
  where
    totalScore = sum [score | (_, _, score) <- records]

filterByAge :: Int -> [Record] -> [Record]
filterByAge minAge = filter (\(_, age, _) -> age >= minAge)

processData :: String -> Int -> (Double, [Record])
processData csvContent minAge = 
  let records = parseCSV csvContent
      filtered = filterByAge minAge records
      avgScore = calculateAverageScore filtered
  in (avgScore, filtered)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)