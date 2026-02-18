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
    windows m ys = take (length ys - m + 1) $ map (take m) (tails ys)
    average zs = sum zs / fromIntegral (length zs)module DataProcessor where

import Data.List (intercalate)
import Data.Maybe (catMaybes)

type Row = [String]
type CSV = [Row]

parseCSV :: String -> CSV
parseCSV content = map (splitOn ',') (lines content)
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitter [[]]
      where
        splitter char acc@(current:rest)
          | char == delimiter = []:acc
          | otherwise = (char:current):rest

filterRows :: (Row -> Bool) -> CSV -> CSV
filterRows predicate = filter predicate

selectColumns :: [Int] -> CSV -> CSV
selectColumns indices = map (mapMaybeByIndex indices)
  where
    mapMaybeByIndex :: [Int] -> Row -> Row
    mapMaybeByIndex idxs row = catMaybes [safeIndex row i | i <- idxs]
    
    safeIndex :: Row -> Int -> Maybe String
    safeIndex row idx
      | idx >= 0 && idx < length row = Just (row !! idx)
      | otherwise = Nothing

toCSVString :: CSV -> String
toCSVString = intercalate "\n" . map (intercalate ",")

processData :: String -> String
processData input = toCSVString processed
  where
    csv = parseCSV input
    filtered = filterRows (\row -> length row > 2) csv
    processed = selectColumns [0, 2, 4] filtered