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
safeHead (x:_) = Just xmodule DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | length xs < n = []
    | otherwise = avg : movingAverage n (tail xs)
    where
        window = take n xs
        avg = sum window / fromIntegral n

smoothData :: Fractional a => Int -> [a] -> [a]
smoothData windowSize dataPoints =
    let padSize = windowSize `div` 2
        padded = replicate padSize (head dataPoints) ++ dataPoints ++ replicate padSize (last dataPoints)
    in movingAverage windowSize padded

processNumericStream :: Fractional a => Int -> [a] -> [a]
processNumericStream windowSize stream =
    if null stream
        then []
        else smoothData windowSize streammodule DataProcessor where

import Data.List.Split (splitOn)

parseCSV :: String -> [[Double]]
parseCSV content = map (map read . splitOn ",") $ lines content

calculateAverages :: [[Double]] -> [Double]
calculateAverages rows = 
    if null rows then []
    else map (\col -> sum col / fromIntegral (length col)) $ transpose rows
  where
    transpose :: [[Double]] -> [[Double]]
    transpose [] = []
    transpose ([]:_) = []
    transpose x = map head x : transpose (map tail x)

processCSVData :: String -> Maybe [Double]
processCSVData content = 
    let parsed = parseCSV content
    in if all (\row -> length row == length (head parsed)) parsed
       then Just $ calculateAverages parsed
       else Nothing