
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData = all (> 0)

main :: IO ()
main = do
    let inputData = [1, -2, 3, 0, 5, -7]
    let processed = processData inputData
    putStrLn $ "Original data: " ++ show inputData
    putStrLn $ "Processed data: " ++ show processed
    putStrLn $ "Data valid: " ++ show (validateData processed)
module DataProcessor where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.Vector (Vector)
import qualified Data.Vector as V

type Row = (String, Double, Double, Double)

parseCSV :: BL.ByteString -> Either String (Vector Row)
parseCSV input = case Csv.decode Csv.NoHeader input of
    Left err -> Left $ "Parse error: " ++ err
    Right rows -> Right $ V.fromList rows

calculateAverages :: Vector Row -> (Double, Double, Double)
calculateAverages rows
    | V.null rows = (0, 0, 0)
    | otherwise = (avg col1, avg col2, avg col3)
  where
    len = fromIntegral $ V.length rows
    col1 = V.map (\(_, a, _, _) -> a) rows
    col2 = V.map (\(_, _, b, _) -> b) rows
    col3 = V.map (\(_, _, _, c) -> c) rows
    avg vec = V.sum vec / len

processData :: BL.ByteString -> Either String (Double, Double, Double)
processData input = do
    parsed <- parseCSV input
    return $ calculateAverages parsed