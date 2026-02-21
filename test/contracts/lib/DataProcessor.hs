module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

data DataRow = DataRow
    { rowId :: Int
    , name  :: String
    , value :: Double
    } deriving (Show, Eq)

parseCSVLine :: String -> Either String DataRow
parseCSVLine line =
    case splitOnComma line of
        [idStr, nameStr, valStr] ->
            case (validateInt idStr, validateDouble valStr) of
                (Right rId, Right val) -> Right $ DataRow rId (trim nameStr) val
                (Left err, _) -> Left $ "Invalid ID: " ++ err
                (_, Left err) -> Left $ "Invalid value: " ++ err
        _ -> Left "Invalid number of columns"

splitOnComma :: String -> [String]
splitOnComma = foldr splitHelper [""]
  where
    splitHelper ',' acc = "":acc
    splitHelper ch (s:ss) = (ch:s):ss
    splitHelper _ [] = error "Unexpected empty list in splitOnComma"

validateInt :: String -> Either String Int
validateInt s
    | all isDigit s = Right (read s)
    | otherwise = Left $ "Not a valid integer: " ++ s

validateDouble :: String -> Either String Double
validateDouble s =
    case reads s of
        [(val, "")] -> Right val
        _ -> Left $ "Not a valid double: " ++ s

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

processCSVData :: [String] -> ([DataRow], [String])
processCSVData lines =
    foldr processLine ([], []) (zip [1..] lines)
  where
    processLine (lineNum, line) (rows, errors) =
        case parseCSVLine line of
            Right row -> (row:rows, errors)
            Left err -> (rows, ("Line " ++ show lineNum ++ ": " ++ err):errors)

formatResults :: ([DataRow], [String]) -> String
formatResults (rows, errors) =
    "Valid rows: " ++ show (length rows) ++ "\n" ++
    "Errors: " ++ show (length errors) ++ "\n" ++
    if null errors
        then "All rows processed successfully\n" ++ formatRows rows
        else "Error details:\n" ++ intercalate "\n" errors ++ "\n"

formatRows :: [DataRow] -> String
formatRows rows =
    intercalate "\n" $ map formatRow rows
  where
    formatRow (DataRow id_ name_ val) =
        "ID: " ++ show id_ ++ ", Name: " ++ name_ ++ ", Value: " ++ show val