module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

import Data.Char (toUpper)

data ValidationError = InvalidLength | InvalidFormat | EmptyInput
    deriving (Show, Eq)

validateInput :: String -> Either ValidationError String
validateInput "" = Left EmptyInput
validateInput s
    | length s < 3 = Left InvalidLength
    | not (all isAlpha s) = Left InvalidFormat
    | otherwise = Right s
    where
        isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

transformData :: String -> String
transformData = map toUpper

processData :: String -> Either ValidationError String
processData input = do
    validated <- validateInput input
    return $ transformData validated

safeProcess :: String -> String
safeProcess input = case processData input of
    Left err -> "Error: " ++ show err
    Right result -> "Processed: " ++ result