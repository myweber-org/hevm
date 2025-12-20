
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers
module DataProcessor where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.Vector (Vector)
import qualified Data.Vector as V

type Row = (String, Double, Double)

parseCSV :: BL.ByteString -> Either String (Vector Row)
parseCSV input = case Csv.decode Csv.NoHeader input of
    Left err -> Left $ "Parse error: " ++ err
    Right rows -> Right $ V.fromList rows

computeAverages :: Vector Row -> (Double, Double)
computeAverages rows = 
    let (sum1, sum2, count) = V.foldl' (\(s1, s2, c) (_, v1, v2) -> (s1 + v1, s2 + v2, c + 1)) (0, 0, 0) rows
    in if count > 0 
        then (sum1 / fromIntegral count, sum2 / fromIntegral count)
        else (0, 0)

processData :: BL.ByteString -> Either String (Double, Double)
processData input = do
    rows <- parseCSV input
    return $ computeAverages rows