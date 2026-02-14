module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Bool
validateInput = all (\x -> x >= -100 && x <= 100)

safeProcessData :: [Int] -> Maybe [Int]
safeProcessData xs
    | validateInput xs = Just (processData xs)
    | otherwise = Nothing
module DataProcessor where

import Data.List.Split (splitOn)

type Record = (String, [Double])

parseCSV :: String -> [Record]
parseCSV content = map parseLine (lines content)
  where
    parseLine line = 
        let parts = splitOn "," line
            name = head parts
            values = map read (tail parts)
        in (name, values)

calculateAverages :: [Record] -> [(String, Double)]
calculateAverages records = map avg records
  where
    avg (name, vals) = 
        let total = sum vals
            count = fromIntegral (length vals)
        in (name, total / count)

processData :: String -> [(String, Double)]
processData = calculateAverages . parseCSV

main :: IO ()
main = do
    let csvData = "Alice,85,90,78\nBob,92,88,95\nCharlie,76,82,79"
    let averages = processData csvData
    mapM_ (\(name, avg) -> putStrLn $ name ++ ": " ++ show avg) averages