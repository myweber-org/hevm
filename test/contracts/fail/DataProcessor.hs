
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -1000) xs then Just xs else Nothing

main :: IO ()
main = do
    let sampleData = [1, -5, 3, 0, 8, -2]
    case validateInput sampleData of
        Just validData -> do
            putStrLn $ "Original data: " ++ show validData
            putStrLn $ "Processed data: " ++ show (processData validData)
            putStrLn $ "Sum of processed data: " ++ show (sumProcessedData validData)
        Nothing -> putStrLn "Input contains values less than -1000"
module DataProcessor where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Record = (String, Double, Double)

parseCSV :: String -> [Record]
parseCSV content = mapMaybe parseLine (lines content)
  where
    parseLine line = case splitOn "," line of
      [name, val1Str, val2Str] -> 
        case (reads val1Str, reads val2Str) of
          ([(val1, "")], [(val2, "")]) -> Just (name, val1, val2)
          _ -> Nothing
      _ -> Nothing

calculateAverages :: [Record] -> (Double, Double)
calculateAverages records = (avg val1s, avg val2s)
  where
    (val1s, val2s) = unzip [(v1, v2) | (_, v1, v2) <- records]
    avg xs = sum xs / fromIntegral (length xs)

processData :: String -> Maybe (Double, Double)
processData content = 
  let records = parseCSV content
  in if null records 
     then Nothing 
     else Just (calculateAverages records)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

import Data.List (tails)

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map avg $ filter (\window -> length window == n) $ tails xs
  where
    avg window = sum window / fromIntegral n

calculateStats :: [Double] -> (Double, Double, Double)
calculateStats xs = (minimum xs, maximum xs, sum xs / fromIntegral (length xs))

smoothData :: Int -> [Double] -> [Double]
smoothData windowSize = movingAverage windowSize

processDataset :: [Double] -> IO ()
processDataset dataset = do
    putStrLn $ "Original data: " ++ show dataset
    putStrLn $ "Smoothed data (window=3): " ++ show (smoothData 3 dataset)
    let (minVal, maxVal, avgVal) = calculateStats dataset
    putStrLn $ "Statistics - Min: " ++ show minVal ++ ", Max: " ++ show maxVal ++ ", Avg: " ++ show avgValmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers