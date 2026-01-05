module DataProcessor where

import Data.List (foldl')
import Text.Read (readMaybe)

type Row = [String]
type CSV = [Row]

parseCSV :: String -> CSV
parseCSV = map (splitOn ',') . lines
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitHelper [[]]
      where
        splitHelper char (current:rest)
          | char == delimiter = []:current:rest
          | otherwise = (char:current):rest

safeReadDouble :: String -> Maybe Double
safeReadDouble = readMaybe

computeColumnStats :: CSV -> [(Double, Double, Double)]
computeColumnStats [] = []
computeColumnStats (header:rows) = 
  map processColumn $ transpose rows
  where
    processColumn :: [String] -> (Double, Double, Double)
    processColumn col = 
      let values = mapMaybe safeReadDouble col
          count = fromIntegral $ length values
          sumVal = foldl' (+) 0 values
          avg = if count > 0 then sumVal / count else 0
          minVal = if null values then 0 else minimum values
          maxVal = if null values then 0 else maximum values
      in (avg, minVal, maxVal)

    transpose :: [[a]] -> [[a]]
    transpose [] = []
    transpose ([]:_) = []
    transpose xss = map head xss : transpose (map tail xss)

    mapMaybe :: (a -> Maybe b) -> [a] -> [b]
    mapMaybe _ [] = []
    mapMaybe f (x:xs) = case f x of
      Just y -> y : mapMaybe f xs
      Nothing -> mapMaybe f xs

printStats :: [(Double, Double, Double)] -> IO ()
printStats stats = do
  putStrLn "Column Statistics (avg, min, max):"
  mapM_ (\(i, (avg, minV, maxV)) -> 
    putStrLn $ "Column " ++ show i ++ ": " ++ 
               show avg ++ ", " ++ show minV ++ ", " ++ show maxV)
    (zip [1..] stats)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Bool
validateInput xs = not (null xs) && all (\x -> x >= -100 && x <= 100) xs

safeProcess :: [Int] -> Maybe Int
safeProcess xs
    | validateInput xs = Just (sumProcessedData xs)
    | otherwise = Nothing