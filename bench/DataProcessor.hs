module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isAlpha)

data ValidationError = InvalidFormat String | MissingField String | InvalidValue String
    deriving (Show, Eq)

type CSVRow = [String]
type Header = [String]

validateCSVRow :: Header -> CSVRow -> Either ValidationError CSVRow
validateCSVRow header row
    | length header /= length row = Left $ InvalidFormat "Column count mismatch"
    | any null row = Left $ MissingField "Empty field detected"
    | not (validateRowData row) = Left $ InvalidValue "Invalid data in row"
    | otherwise = Right row

validateRowData :: CSVRow -> Bool
validateRowData = all validateField
    where
        validateField field
            | all isDigit field = True
            | all isAlpha field = True
            | ':' `elem` field = validateTimeFormat field
            | otherwise = False

        validateTimeFormat str =
            case break (== ':') str of
                (hours, ':' : minutes) ->
                    let hourDigits = take 2 hours
                        minuteDigits = take 2 minutes
                    in all isDigit hourDigits && 
                       all isDigit minuteDigits &&
                       length hourDigits == 2 &&
                       length minuteDigits == 2
                _ -> False

processCSVData :: Header -> [CSVRow] -> ([CSVRow], [ValidationError])
processCSVData header rows = foldr processRow ([], []) rows
    where
        processRow row (validRows, errors) =
            case validateCSVRow header row of
                Left err -> (validRows, err : errors)
                Right validRow -> (validRow : validRows, errors)

formatErrors :: [ValidationError] -> String
formatErrors errors = intercalate "\n" $ map formatError errors
    where
        formatError (InvalidFormat msg) = "Format error: " ++ msg
        formatError (MissingField msg) = "Missing field: " ++ msg
        formatError (InvalidValue msg) = "Invalid value: " ++ msg

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

validateHeader :: Header -> Bool
validateHeader header = not (null header) && length header <= 20 && all (not . null) header