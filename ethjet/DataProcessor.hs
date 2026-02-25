module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

import Data.List (sort, group)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.Vector (Vector, toList)

type Record = (String, Int, Double)

parseCSV :: FilePath -> IO (Either String (Vector Record))
parseCSV filepath = do
    csvData <- BL.readFile filepath
    return $ Csv.decode Csv.NoHeader csvData

calculateMean :: [Double] -> Double
calculateMean xs = sum xs / fromIntegral (length xs)

calculateMedian :: [Double] -> Double
calculateMedian xs = 
    let sorted = sort xs
        n = length sorted
        mid = n `div` 2
    in if odd n 
        then sorted !! mid
        else (sorted !! (mid - 1) + sorted !! mid) / 2

calculateMode :: [Int] -> [Int]
calculateMode xs = 
    let grouped = group $ sort xs
        maxFreq = maximum $ map length grouped
    in map head $ filter (\g -> length g == maxFreq) grouped

filterByThreshold :: Double -> [Record] -> [Record]
filterByThreshold threshold = filter (\(_, _, value) -> value > threshold)

aggregateByCategory :: [Record] -> [(String, Double)]
aggregateByCategory records =
    let groups = groupByCategory records
    in map (\(cat, vals) -> (cat, calculateMean vals)) groups
    where
        groupByCategory :: [Record] -> [(String, [Double])]
        groupByCategory = foldr (\(cat, _, val) acc ->
            case lookup cat acc of
                Just vals -> (cat, val:vals) : filter (\(c,_) -> c /= cat) acc
                Nothing -> (cat, [val]) : acc) []

processData :: FilePath -> Double -> IO ()
processData filepath threshold = do
    result <- parseCSV filepath
    case result of
        Left err -> putStrLn $ "Error parsing CSV: " ++ err
        Right records -> do
            let filtered = filterByThreshold threshold $ toList records
            let values = map (\(_, _, val) -> val) filtered
            let categories = aggregateByCategory filtered
            
            putStrLn $ "Total records: " ++ show (length $ toList records)
            putStrLn $ "Filtered records: " ++ show (length filtered)
            putStrLn $ "Mean value: " ++ show (calculateMean values)
            putStrLn $ "Median value: " ++ show (calculateMedian values)
            
            mapM_ (\(cat, avg) -> 
                putStrLn $ "Category " ++ cat ++ " average: " ++ show avg) categories