module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show result
module DataProcessor where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.Vector (Vector)
import qualified Data.Vector as V

type Record = (String, Double, Double)

parseCSV :: BL.ByteString -> Either String (Vector Record)
parseCSV input = case Csv.decode Csv.NoHeader input of
    Left err -> Left $ "Parse error: " ++ err
    Right records -> Right $ V.fromList records

calculateAverages :: Vector Record -> (Double, Double)
calculateAverages records
    | V.null records = (0.0, 0.0)
    | otherwise = (avg col1, avg col2)
  where
    col1 = V.map (\(_, x, _) -> x) records
    col2 = V.map (\(_, _, y) -> y) records
    avg xs = V.sum xs / fromIntegral (V.length xs)

processData :: BL.ByteString -> Either String (Double, Double)
processData input = do
    records <- parseCSV input
    return $ calculateAverages records