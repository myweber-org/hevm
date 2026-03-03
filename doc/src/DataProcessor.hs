module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processDatamodule DataProcessor where

import Data.List (intercalate)
import Text.Read (readMaybe)

data ValidationError = InvalidRow Int String | MissingField Int String
    deriving (Show, Eq)

type CSVRow = [String]
type ValidatedRow = Either ValidationError [(String, String)]

parseCSV :: String -> [CSVRow]
parseCSV content = map (splitOn ',') (lines content)
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitHelper [""]
      where
        splitHelper char acc@(current:rest)
          | char == delimiter = "":acc
          | otherwise = (char:current):rest

validateRow :: Int -> CSVRow -> ValidatedRow
validateRow rowNum fields
  | length fields < 2 = Left (MissingField rowNum "Insufficient columns")
  | any null fields = Left (InvalidRow rowNum "Empty field detected")
  | not (isNumeric (fields !! 1)) = Left (InvalidRow rowNum "Second column must be numeric")
  | otherwise = Right (zip ["id", "value"] fields)
  where
    isNumeric :: String -> Bool
    isNumeric str = case readMaybe str :: Maybe Double of
        Just _ -> True
        Nothing -> False

processCSVData :: String -> Either String [(String, String)]
processCSVData content = 
    let rows = parseCSV content
        validated = zipWith validateRow [1..] rows
    in case partitionEithers validated of
        ([], goodRows) -> Right (concat goodRows)
        (errors, _) -> Left (formatErrors errors)
  where
    partitionEithers :: [Either a b] -> ([a], [b])
    partitionEithers = foldr (either left right) ([], [])
      where
        left a (ls, rs) = (a:ls, rs)
        right b (ls, rs) = (ls, b:rs)
    
    formatErrors :: [ValidationError] -> String
    formatErrors errs = "Validation errors:\n" ++ intercalate "\n" (map show errs)

sampleData :: String
sampleData = intercalate "\n"
    [ "A001,42.5"
    , "B002,17.8"
    , "C003,invalid"
    , "D004"
    , "E005,99.9"
    ]