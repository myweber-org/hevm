
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
    | otherwise = Nothingmodule DataProcessor where

import Data.List.Split (splitOn)
import qualified Data.Map as Map

type Record = Map.Map String String

parseCSV :: String -> [Record]
parseCSV content = 
    let lines' = filter (not . null) $ lines content
        headers = splitOn "," $ head lines'
        rows = map (splitOn ",") $ tail lines'
    in map (Map.fromList . zip headers) rows

computeAverage :: String -> [Record] -> Maybe Double
computeAverage field records = 
    let values = mapMaybe (Map.lookup field) records
        parsed = mapMaybe safeRead values
    in if null parsed 
        then Nothing 
        else Just (sum parsed / fromIntegral (length parsed))
    where
        safeRead s = case reads s of
            [(x, "")] -> Just x
            _ -> Nothing

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr (\x acc -> case f x of
    Just y -> y : acc
    Nothing -> acc) []