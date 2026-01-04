module DataProcessor where

import Data.List (tails)

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | n > length xs = []
    | otherwise = map average $ filter (\w -> length w == n) $ tails xs
  where
    average ws = sum ws / fromIntegral n

smoothData :: [Double] -> [Double]
smoothData = movingAverage 3

main :: IO ()
main = do
    let sampleData = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
    putStrLn "Original data:"
    print sampleData
    putStrLn "\nSmoothed data (3-point moving average):"
    print $ smoothData sampleDatamodule DataProcessor where

import Data.Char (toUpper)

-- Validate that a string is non-empty and contains only alphabetic characters
validateName :: String -> Maybe String
validateName "" = Nothing
validateName name
    | all isAlpha name = Just name
    | otherwise = Nothing
  where
    isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

-- Convert a string to title case
toTitleCase :: String -> String
toTitleCase = unwords . map capitalize . words
  where
    capitalize [] = []
    capitalize (x:xs) = toUpper x : map toLower xs
    toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

-- Process a list of names: validate, transform to title case, and filter valid ones
processNames :: [String] -> [String]
processNames = map toTitleCase . filterNames
  where
    filterNames = foldr (\name acc -> case validateName name of
        Just validName -> validName : acc
        Nothing -> acc) []

-- Calculate average length of valid names
averageNameLength :: [String] -> Maybe Double
averageNameLength names =
    let validNames = map toTitleCase $ filterNames names
        totalLength = sum $ map length validNames
        count = length validNames
    in if count > 0
        then Just (fromIntegral totalLength / fromIntegral count)
        else Nothing
  where
    filterNames = foldr (\name acc -> case validateName name of
        Just validName -> validName : acc
        Nothing -> acc) []module DataProcessor where

import Data.List (intercalate)
import Text.Read (readMaybe)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> Either String CSVData
parseCSV content = 
    let rows = lines content
        parsedRows = map (splitOn ',') rows
    in if allValid parsedRows
        then Right parsedRows
        else Left "Invalid CSV format: empty rows or inconsistent columns"

splitOn :: Char -> String -> Row
splitOn delimiter = foldr splitter [[]]
  where
    splitter char (current:rest)
        | char == delimiter = []:current:rest
        | otherwise = (char:current):rest

allValid :: CSVData -> Bool
allValid [] = True
allValid (first:rest) = 
    let colCount = length first
    in colCount > 0 && all (\row -> length row == colCount && not (null row)) rest

validateNumericColumns :: CSVData -> [Int] -> Either String CSVData
validateNumericColumns data columns = 
    case invalidRows of
        [] -> Right data
        _ -> Left $ "Non-numeric values in columns " ++ show columns ++ " at rows " ++ show invalidRows
  where
    invalidRows = [i | (i, row) <- zip [1..] data, any (not . isNumericAt row) columns]
    isNumericAt row idx = 
        if idx >= 1 && idx <= length row
        then isNumeric (row !! (idx - 1))
        else False
    isNumeric str = case readMaybe str :: Maybe Double of
        Just _ -> True
        Nothing -> False

processCSV :: String -> [Int] -> Either String CSVData
processCSV content numericCols = do
    parsed <- parseCSV content
    validateNumericColumns parsed numericCols

formatOutput :: CSVData -> String
formatOutput = intercalate "\n" . map (intercalate "|")