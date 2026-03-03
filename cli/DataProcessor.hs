module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

main :: IO ()
main = do
    let numbers = [-3, 1, 4, -1, 5, 9, -2, 6]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)
module DataProcessor where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Record = (String, Double, Double)

parseCSVLine :: String -> Maybe Record
parseCSVLine line = 
    case splitOn "," line of
        [name, val1, val2] -> 
            case (reads val1, reads val2) of
                ([(v1, "")], [(v2, "")]) -> Just (name, v1, v2)
                _ -> Nothing
        _ -> Nothing

calculateAverages :: [Record] -> (Double, Double)
calculateAverages records =
    let (sum1, sum2) = foldr (\(_, v1, v2) (s1, s2) -> (s1 + v1, s2 + v2)) (0, 0) records
        count = fromIntegral (length records)
    in (sum1 / count, sum2 / count)

processCSVData :: String -> Maybe (Double, Double)
processCSVData csvContent =
    let records = mapMaybe parseCSVLine (lines csvContent)
    in if null records 
        then Nothing
        else Just (calculateAverages records)

validateRecord :: Record -> Bool
validateRecord (_, v1, v2) = v1 >= 0 && v2 >= 0

filterValidRecords :: [Record] -> [Record]
filterValidRecords = filter validateRecord

processAndFilterCSV :: String -> Maybe (Double, Double)
processAndFilterCSV csvContent =
    let records = mapMaybe parseCSVLine (lines csvContent)
        validRecords = filterValidRecords records
    in if null validRecords
        then Nothing
        else Just (calculateAverages validRecords)