module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isAlpha)
import Control.Monad (when)

data ValidationError = InvalidFormat String | MissingField String | InvalidValue String
    deriving (Show, Eq)

type CSVRow = [String]
type ValidatedRow = Either ValidationError CSVRow

validateRow :: CSVRow -> ValidatedRow
validateRow row = do
    when (length row < 3) $ Left (MissingField "Row must contain at least 3 fields")
    validateId (row !! 0)
    validateName (row !! 1)
    validateAge (row !! 2)
    return row

validateId :: String -> ValidatedRow
validateId idStr
    | all isDigit idStr && length idStr == 5 = Right []
    | otherwise = Left (InvalidValue $ "Invalid ID format: " ++ idStr)

validateName :: String -> ValidatedRow
validateName name
    | all isAlpha name && length name >= 2 = Right []
    | otherwise = Left (InvalidValue $ "Invalid name: " ++ name)

validateAge :: String -> ValidatedRow
validateAge ageStr = case reads ageStr :: [(Int, String)] of
    [(age, "")] -> if age >= 18 && age <= 120 then Right [] else Left (InvalidValue $ "Age out of range: " ++ ageStr)
    _ -> Left (InvalidValue $ "Invalid age format: " ++ ageStr)

processCSVData :: [CSVRow] -> ([CSVRow], [ValidationError])
processCSVData rows = foldr processRow ([], []) rows
    where
        processRow row (validRows, errors) = case validateRow row of
            Left err -> (validRows, err : errors)
            Right validRow -> (validRow : validRows, errors)

formatErrors :: [ValidationError] -> String
formatErrors errors = intercalate "\n" $ map show errors

sampleData :: [CSVRow]
sampleData =
    [ ["12345", "Alice", "25"]
    , ["67890", "Bob", "30"]
    , ["1234", "Charlie", "35"]
    , ["54321", "D1", "150"]
    , ["98765", "Eve", "seventeen"]
    ]