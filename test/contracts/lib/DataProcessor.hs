module DataProcessor where

import Data.List (sort, group)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

type Dataset = [(String, Double)]

parseCSVLine :: String -> Maybe (String, Double)
parseCSVLine line = case words line of
    [label, valueStr] -> case reads valueStr of
        [(value, "")] -> Just (label, value)
        _ -> Nothing
    _ -> Nothing

loadDataset :: String -> IO Dataset
loadDataset filename = do
    content <- readFile filename
    let linesOfFile = lines content
    return $ mapMaybe parseCSVLine linesOfFile
  where
    mapMaybe f = foldr (\x acc -> case f x of
        Just val -> val : acc
        Nothing -> acc) []

calculateMean :: Dataset -> Double
calculateMean dataset = 
    let values = map snd dataset
        total = sum values
        count = fromIntegral (length values)
    in total / count

calculateMedian :: Dataset -> Double
calculateMedian dataset = 
    let values = sort $ map snd dataset
        len = length values
        mid = len `div` 2
    in if even len
        then (values !! (mid - 1) + values !! mid) / 2
        else values !! mid

calculateMode :: Dataset -> [Double]
calculateMode dataset = 
    let valueGroups = group $ sort $ map snd dataset
        maxFreq = maximum $ map length valueGroups
    in map head $ filter (\g -> length g == maxFreq) valueGroups

frequencyDistribution :: Dataset -> Map.Map Double Int
frequencyDistribution dataset = 
    foldr (\val acc -> Map.insertWith (+) val 1 acc) Map.empty (map snd dataset)

filterByThreshold :: Double -> Dataset -> Dataset
filterByThreshold threshold = filter (\(_, val) -> val > threshold)

normalizeDataset :: Dataset -> Dataset
normalizeDataset dataset = 
    let values = map snd dataset
        maxVal = maximum values
        minVal = minimum values
        range = maxVal - minVal
    in if range == 0
        then dataset
        else map (\(label, val) -> (label, (val - minVal) / range)) dataset

processData :: String -> IO ()
processData filename = do
    dataset <- loadDataset filename
    putStrLn $ "Loaded " ++ show (length dataset) ++ " data points"
    putStrLn $ "Mean: " ++ show (calculateMean dataset)
    putStrLn $ "Median: " ++ show (calculateMedian dataset)
    putStrLn $ "Mode(s): " ++ show (calculateMode dataset)
    putStrLn "Frequency distribution:"
    mapM_ (\(val, freq) -> putStrLn $ "  " ++ show val ++ ": " ++ show freq) 
          (Map.toList $ frequencyDistribution dataset)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let input = [1..10]
    let result = processData input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result