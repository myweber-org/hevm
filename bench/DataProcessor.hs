module DataProcessor where

import Data.List (transpose)
import Text.Read (readMaybe)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> Either String CSVData
parseCSV content = mapM parseRow (lines content)
  where
    parseRow line = case splitOnComma line of
        [] -> Left "Empty row found"
        cells -> Right cells

    splitOnComma = foldr splitHelper [""]
    splitHelper ',' acc = "":acc
    splitHelper chr (x:xs) = (chr:x):xs
    splitHelper _ [] = error "Unexpected empty list in splitHelper"

validateCSV :: CSVData -> Either String CSVData
validateCSV [] = Right []
validateCSV rows@(firstRow:_)
    | all (\r -> length r == colCount) rows = Right rows
    | otherwise = Left $ "Inconsistent column count. Expected " ++ show colCount
  where
    colCount = length firstRow

convertColumn :: (String -> Maybe a) -> Row -> Either String [a]
convertColumn parser row = mapM (\cell -> maybeToEither ("Failed to parse: " ++ cell) (parser cell)) row
  where
    maybeToEither err Nothing = Left err
    maybeToEither _ (Just val) = Right val

processNumericCSV :: String -> Either String [[Double]]
processNumericCSV content = do
    parsed <- parseCSV content
    validated <- validateCSV parsed
    mapM (convertColumn readMaybe) validated
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result