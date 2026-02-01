module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows m ys = take (length ys - m + 1) $ zipWith (++) (tails ys) (repeat [])
    average zs = sum zs / fromIntegral (length zs)module DataProcessor where

processNumbers :: [Int] -> [Int]
processNumbers = map (*2) . filter (>0)module DataProcessor where

import Data.List (foldl')
import Text.Read (readMaybe)

data StatRecord = StatRecord
    { recordId :: Int
    , value    :: Double
    } deriving (Show, Eq)

parseCSVLine :: String -> Maybe StatRecord
parseCSVLine line = case words line of
    [idStr, valStr] -> do
        idVal <- readMaybe idStr
        dVal  <- readMaybe valStr
        return $ StatRecord idVal dVal
    _ -> Nothing

parseCSVData :: String -> [StatRecord]
parseCSVData = mapMaybe parseCSVLine . lines
  where
    mapMaybe f = foldr (\x acc -> case f x of
        Just val -> val : acc
        Nothing  -> acc) []

computeStats :: [StatRecord] -> (Double, Double, Double)
computeStats records =
    let values = map value records
        sumVal = foldl' (+) 0 values
        len = fromIntegral $ length values
        mean = sumVal / len
        variance = foldl' (\acc v -> acc + (v - mean) ** 2) 0 values / len
        stdDev = sqrt variance
    in (mean, variance, stdDev)

processCSV :: String -> Maybe (Double, Double, Double)
processCSV input =
    let records = parseCSVData input
    in if null records
        then Nothing
        else Just $ computeStats records
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbers