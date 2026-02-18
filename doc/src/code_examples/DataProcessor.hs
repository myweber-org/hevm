
module DataProcessor where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.Vector (Vector)
import qualified Data.Vector as V

type Row = (String, Double, Double, Double)

parseCSV :: BL.ByteString -> Either String (Vector Row)
parseCSV input = do
    decoded <- Csv.decode Csv.NoHeader input
    return $ V.map toRow decoded
  where
    toRow (name, val1, val2, val3) = (name, val1, val2, val3)

calculateAverages :: Vector Row -> (Double, Double, Double)
calculateAverages rows
    | V.null rows = (0, 0, 0)
    | otherwise = (avg1, avg2, avg3)
  where
    len = fromIntegral $ V.length rows
    (total1, total2, total3) = V.foldl' sumTriplet (0, 0, 0) rows
    sumTriplet (a1, a2, a3) (_, v1, v2, v3) = (a1 + v1, a2 + v2, a3 + v3)
    avg1 = total1 / len
    avg2 = total2 / len
    avg3 = total3 / len

processData :: BL.ByteString -> Either String (Double, Double, Double)
processData input = do
    rows <- parseCSV input
    return $ calculateAverages rows