
module DataProcessor where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)

newtype ValidatedString = ValidatedString String
    deriving (Show, Eq)

data ValidationError = 
      EmptyString
    | ContainsSpaces
    | InvalidCharacters
    | LengthExceeded Int
    deriving (Show, Eq)

validateNonEmpty :: String -> Either ValidationError ValidatedString
validateNonEmpty "" = Left EmptyString
validateNonEmpty str = Right (ValidatedString str)

validateNoSpaces :: String -> Either ValidationError ValidatedString
validateNoSpaces str
    | any isSpace str = Left ContainsSpaces
    | otherwise = Right (ValidatedString str)

validateNumeric :: String -> Either ValidationError ValidatedString
validateNumeric str
    | all isDigit str = Right (ValidatedString str)
    | otherwise = Left InvalidCharacters

validateMaxLength :: Int -> String -> Either ValidationError ValidatedString
validateMaxLength maxLen str
    | length str > maxLen = Left (LengthExceeded maxLen)
    | otherwise = Right (ValidatedString str)

composeValidators :: [String -> Either ValidationError ValidatedString] -> String -> Either [ValidationError] ValidatedString
composeValidators validators input =
    case partitionEithers results of
        ([], [ValidatedString _]) -> Right (ValidatedString input)
        (errors, _) -> Left errors
    where
        results = map ($ input) validators
        partitionEithers = foldr select ([], [])
        select (Left err) (errs, vals) = (err:errs, vals)
        select (Right val) (errs, vals) = (errs, val:vals)

transformToUpper :: ValidatedString -> String
transformToUpper (ValidatedString str) = map toUpper str

transformToLower :: ValidatedString -> String
transformToLower (ValidatedString str) = map toLower str

padString :: Int -> Char -> ValidatedString -> String
padString width padChar (ValidatedString str) =
    if length str >= width
        then str
        else str ++ replicate (width - length str) padChar

parseCommaSeparated :: String -> [String]
parseCommaSeparated = filter (not . null) . map (dropWhile isSpace) . splitBy ','

splitBy :: Char -> String -> [String]
splitBy delimiter = foldr splitHelper [""]
    where
        splitHelper ch (current:rest)
            | ch == delimiter = "":current:rest
            | otherwise = (ch:current):rest

safeReadInt :: String -> Maybe Int
safeReadInt str = 
    case reads str of
        [(n, "")] -> Just n
        _ -> Nothing

batchProcess :: (a -> Maybe b) -> [a] -> [b]
batchProcess processor = catMaybes . map processor

formatErrors :: [ValidationError] -> String
formatErrors errors = 
    "Validation failed: " ++ intercalate ", " (map describeError errors)
    where
        describeError EmptyString = "string cannot be empty"
        describeError ContainsSpaces = "string contains spaces"
        describeError InvalidCharacters = "invalid characters detected"
        describeError (LengthExceeded n) = "exceeds maximum length of " ++ show n

processInput :: String -> Either String String
processInput input = 
    case composeValidators [validateNonEmpty, validateNoSpaces, validateMaxLength 50] input of
        Left errs -> Left (formatErrors errs)
        Right validated -> Right (transformToUpper validated)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 10) (* 2)

validateData :: [Int] -> Bool
validateData = all (\x -> x `mod` 2 == 0) . processData