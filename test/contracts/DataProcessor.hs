module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isAlpha)
import Control.Monad (when)

type CSVRow = [String]
type ValidationError = String

validateCSVRow :: CSVRow -> Either ValidationError CSVRow
validateCSVRow [] = Left "Empty row"
validateCSVRow row = do
    when (length row < 3) $ Left "Row must have at least 3 columns"
    validateId (head row)
    validateName (row !! 1)
    validateAge (row !! 2)
    return row

validateId :: String -> Either ValidationError String
validateId idStr
    | all isDigit idStr && length idStr == 5 = Right idStr
    | otherwise = Left $ "Invalid ID format: " ++ idStr

validateName :: String -> Either ValidationError String
validateName name
    | all isAlpha name && length name >= 2 = Right name
    | otherwise = Left $ "Invalid name: " ++ name

validateAge :: String -> Either ValidationError String
validateAge ageStr = case reads ageStr :: [(Int, String)] of
    [(age, "")] -> if age >= 18 && age <= 120
                   then Right ageStr
                   else Left $ "Age out of range: " ++ ageStr
    _ -> Left $ "Invalid age format: " ++ ageStr

processCSVData :: [CSVRow] -> Either ValidationError [CSVRow]
processCSVData rows = mapM validateCSVRow rows

formatResults :: Either ValidationError [CSVRow] -> String
formatResults (Left err) = "Validation failed: " ++ err
formatResults (Right rows) = "Validated " ++ show (length rows) ++ " rows:\n" ++
    intercalate "\n" (map (intercalate ",") rows)