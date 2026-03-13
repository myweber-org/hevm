module DataProcessor where

import Data.List (intercalate)
import Text.Read (readMaybe)

data ValidationError = InvalidFormat String | InvalidValue String
    deriving (Show, Eq)

type CSVRow = [String]
type ValidatedRow = Either ValidationError [Double]

parseCSV :: String -> [CSVRow]
parseCSV content = map (splitOn ',') (lines content)
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitHelper [""]
      where
        splitHelper char acc@(current:rest)
            | char == delimiter = "":acc
            | otherwise = (char:current):rest

validateRow :: CSVRow -> ValidatedRow
validateRow row = case traverse validateCell row of
    Just values -> Right values
    Nothing -> Left (InvalidFormat $ "Row contains non-numeric values: " ++ show row)
  where
    validateCell :: String -> Maybe Double
    validateCell cell = readMaybe cell

processCSVData :: String -> Either ValidationError [[Double]]
processCSVData content = 
    let rows = parseCSV content
        validatedRows = map validateRow rows
    in case sequence validatedRows of
        Right values -> Right values
        Left err -> Left err

calculateStatistics :: [[Double]] -> (Double, Double, Double)
calculateStatistics rows = 
    let allValues = concat rows
        count = fromIntegral (length allValues)
        sumValues = sum allValues
        mean = sumValues / count
        variance = sum (map (\x -> (x - mean) ** 2) allValues) / count
        stdDev = sqrt variance
    in (mean, variance, stdDev)

main :: IO ()
main = do
    let csvData = "1.5,2.3,3.7\n4.1,5.2,6.8\n7.3,8.9,9.4"
    case processCSVData csvData of
        Right validatedData -> do
            let (mean, variance, stdDev) = calculateStatistics validatedData
            putStrLn $ "Processed " ++ show (length validatedData) ++ " rows"
            putStrLn $ "Mean: " ++ show mean
            putStrLn $ "Variance: " ++ show variance
            putStrLn $ "Standard Deviation: " ++ show stdDev
        Left err -> putStrLn $ "Error processing CSV: " ++ show err