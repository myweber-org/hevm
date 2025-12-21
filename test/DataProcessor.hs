
module DataProcessor where

import Data.Char (isDigit, isAlpha)
import Data.List (intercalate)

type ValidationRule = String -> Bool
type Transformation = String -> String

validateNumeric :: ValidationRule
validateNumeric = all isDigit

validateAlpha :: ValidationRule
validateAlpha = all isAlpha

validateLength :: Int -> ValidationRule
validateLength n s = length s == n

transformToUpper :: Transformation
transformToUpper = map toUpper

transformPadLeft :: Int -> Char -> Transformation
transformPadLeft n c s = replicate (n - length s) c ++ s

processField :: ValidationRule -> Transformation -> String -> Maybe String
processField validate transform input =
    if validate input
        then Just $ transform input
        else Nothing

processCSVRow :: [String] -> [ValidationRule] -> [Transformation] -> Maybe [String]
processCSVRow row validations transforms =
    sequence $ zipWith3 processField validations transforms row

validateCSV :: [[String]] -> [ValidationRule] -> [Transformation] -> ([[String]], [String])
validateCSV rows validations transforms =
    foldr processRow ([], []) rows
  where
    processRow row (success, errors) =
        case processCSVRow row validations transforms of
            Just processed -> (processed:success, errors)
            Nothing -> (success, show row : errors)

formatErrors :: [String] -> String
formatErrors errors =
    "Validation failed for rows:\n" ++ intercalate "\n" errors

sampleData :: [[String]]
sampleData =
    [ ["123", "abc", "2023"]
    , ["456", "def", "2024"]
    , ["78x", "ghi", "2025"]
    ]

sampleValidations :: [ValidationRule]
sampleValidations =
    [ validateNumeric
    , validateAlpha
    , validateLength 4
    ]

sampleTransforms :: [Transformation]
sampleTransforms =
    [ transformPadLeft 5 '0'
    , transformToUpper
    , id
    ]