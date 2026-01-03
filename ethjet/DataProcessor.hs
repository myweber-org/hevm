module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = all (\x -> x >= -100 && x <= 100) xs

safeProcess :: [Int] -> Maybe [Int]
safeProcess xs
    | validateInput xs = Just (processData xs)
    | otherwise = Nothing
module DataProcessor where

import Data.List (intercalate)
import Text.Read (readMaybe)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV content = 
    if null content
    then Left "Empty CSV content"
    else Right $ map (splitOn ',') (lines content)
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitHelper [""]
      where
        splitHelper char acc@(current:rest)
            | char == delimiter = "":acc
            | otherwise = (char:current):rest

validateNumericColumn :: CSVData -> Int -> Either String [Double]
validateNumericColumn rows columnIndex
    | null rows = Left "No data rows"
    | columnIndex < 0 = Left "Negative column index"
    | otherwise = 
        let values = map (getColumn columnIndex) (drop 1 rows)
        in traverse parseDouble values
  where
    getColumn idx row
        | idx < length row = row !! idx
        | otherwise = ""
    
    parseDouble :: String -> Either String Double
    parseDouble str = 
        case readMaybe str of
            Just num -> Right num
            Nothing -> Left $ "Invalid numeric value: " ++ str

calculateColumnStats :: [Double] -> (Double, Double, Double)
calculateColumnStats values
    | null values = (0, 0, 0)
    | otherwise = (minimum values, maximum values, sum values / fromIntegral (length values))

processCSVFile :: String -> Int -> IO ()
processCSVFile filePath columnIndex = do
    content <- readFile filePath
    case parseCSV content of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right csvData -> 
            case validateNumericColumn csvData columnIndex of
                Left err -> putStrLn $ "Validation error: " ++ err
                Right numericValues -> do
                    let (minVal, maxVal, avgVal) = calculateColumnStats numericValues
                    putStrLn $ "Column " ++ show columnIndex ++ " statistics:"
                    putStrLn $ "  Minimum: " ++ show minVal
                    putStrLn $ "  Maximum: " ++ show maxVal
                    putStrLn $ "  Average: " ++ show avgVal
                    putStrLn $ "  Valid rows processed: " ++ show (length numericValues)