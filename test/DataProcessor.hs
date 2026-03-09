module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers
module DataProcessor where

import Data.List (sort, nub)

-- Filter even numbers from a list
filterEvens :: [Int] -> [Int]
filterEvens = filter even

-- Remove duplicates and sort a list
uniqueSorted :: Ord a => [a] -> [a]
uniqueSorted = sort . nub

-- Transform list by applying function to each element and filtering
transformAndFilter :: (a -> b) -> (b -> Bool) -> [a] -> [b]
transformAndFilter f pred = filter pred . map f

-- Calculate statistics for a list of numbers
data Stats = Stats {
    mean :: Double,
    median :: Double,
    range :: Double
} deriving (Show, Eq)

computeStats :: [Double] -> Maybe Stats
computeStats [] = Nothing
computeStats xs = Just Stats {
    mean = sum xs / fromIntegral (length xs),
    median = calculateMedian sorted,
    range = maximum xs - minimum xs
}
  where
    sorted = sort xs
    calculateMedian ys
        | odd len = ys !! mid
        | otherwise = (ys !! (mid - 1) + ys !! mid) / 2
      where
        len = length ys
        mid = len `div` 2

-- Process data pipeline: filter, transform, and compute statistics
processDataPipeline :: [Double] -> Maybe Stats
processDataPipeline = computeStats . map (* 2) . filter (> 0)