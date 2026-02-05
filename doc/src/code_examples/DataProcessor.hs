
module DataProcessor where

import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)

type Record = (String, Double, Double)

parseCSV :: String -> [Record]
parseCSV content = catMaybes $ map parseLine (lines content)
  where
    parseLine line = case splitOn "," line of
      [name, val1, val2] -> 
        case (reads val1, reads val2) of
          ([(v1, "")], [(v2, "")]) -> Just (name, v1, v2)
          _ -> Nothing
      _ -> Nothing

calculateAverages :: [Record] -> (Double, Double)
calculateAverages records = (avg val1s, avg val2s)
  where
    (val1s, val2s) = unzip $ map (\(_, v1, v2) -> (v1, v2)) records
    avg xs = sum xs / fromIntegral (length xs)

processData :: String -> (Double, Double)
processData = calculateAverages . parseCSV

filterByThreshold :: Double -> [Record] -> [Record]
filterByThreshold threshold = filter (\(_, v1, v2) -> v1 > threshold && v2 > threshold)
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
    let numbers = [-3, -1, 0, 2, 5, 8]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)module DataProcessor where

import Data.List (foldl')
import Text.Read (readMaybe)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ',') (lines content)
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitter [""]
      where
        splitter char acc@(x:xs)
          | char == delimiter = "":acc
          | otherwise = (char:x):xs

safeReadDouble :: String -> Maybe Double
safeReadDouble = readMaybe

calculateColumnAverage :: CSVData -> Int -> Maybe Double
calculateColumnAverage rows columnIndex
  | null validValues = Nothing
  | otherwise = Just (sum validValues / fromIntegral (length validValues))
  where
    validValues = [value | row <- rows,
                           columnIndex < length row,
                           Just value <- [safeReadDouble (row !! columnIndex)]]

processCSVFile :: String -> Int -> IO (Maybe Double)
processCSVFile filePath columnIndex = do
  content <- readFile filePath
  let parsedData = parseCSV content
  return $ calculateColumnAverage parsedData columnIndex