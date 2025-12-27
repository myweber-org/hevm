module DataProcessor where

import Data.Char (isDigit, isAlpha)
import Data.List (intercalate)

data ValidationError = InvalidFormat String | MissingField String
    deriving (Show, Eq)

validatePhone :: String -> Either ValidationError String
validatePhone phone
    | all isDigit phone && length phone == 10 = Right phone
    | otherwise = Left $ InvalidFormat "Phone must be 10 digits"

validateEmail :: String -> Either ValidationError String
validateEmail email
    | '@' `elem` email && '.' `elem` (dropWhile (/= '@') email) = Right email
    | otherwise = Left $ InvalidFormat "Invalid email format"

transformName :: String -> String
transformName = unwords . map capitalize . words
  where
    capitalize [] = []
    capitalize (x:xs) = toUpper x : map toLower xs
    toUpper = toEnum . (+ (-32)) . fromEnum
    toLower c = if c >= 'A' && c <= 'Z'
                then toEnum (fromEnum c + 32)
                else c

processRecord :: [(String, String)] -> Either ValidationError [(String, String)]
processRecord fields = do
    phone <- maybe (Left $ MissingField "phone") validatePhone $ lookup "phone" fields
    email <- maybe (Left $ MissingField "email") validateEmail $ lookup "email" fields
    let name = maybe "" transformName $ lookup "name" fields
    return [("name", name), ("phone", phone), ("email", email)]

formatCSV :: [[(String, String)]] -> String
formatCSV records = 
    let headers = ["name", "phone", "email"]
        headerLine = intercalate "," headers
        recordLines = map (intercalate "," . map snd) records
    in unlines $ headerLine : recordLines