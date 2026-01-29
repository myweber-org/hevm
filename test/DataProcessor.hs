
module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isSpace)
import Control.Applicative ((<|>))

data ValidationError = InvalidFormat String
                     | MissingField String
                     | InvalidValue String String
                     deriving (Eq, Show)

type CSVRow = [String]
type Header = [String]

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

validateRowLength :: Header -> CSVRow -> Either ValidationError CSVRow
validateRowLength header row
  | length header == length row = Right row
  | otherwise = Left $ InvalidFormat 
      ("Expected " ++ show (length header) ++ 
       " fields, got " ++ show (length row))

validateNumericField :: String -> String -> Either ValidationError String
validateNumericField fieldName value
  | all isDigit (trim value) = Right value
  | otherwise = Left $ InvalidValue fieldName 
      ("Non-numeric value: " ++ value)

parseCSVRow :: String -> CSVRow
parseCSVRow = splitByComma . trim
  where
    splitByComma [] = []
    splitByComma str = 
      let (cell, rest) = break (== ',') str
      in trim cell : case rest of
                       ',' : xs -> splitByComma xs
                       _        -> []

validateCSVData :: Header -> [String] -> [Either ValidationError CSVRow]
validateCSVData header = map validateRow
  where
    validateRow raw = do
      let row = parseCSVRow raw
      validatedRow <- validateRowLength header row
      case lookupField "age" header validatedRow of
        Just age -> do
          _ <- validateNumericField "age" age
          return validatedRow
        Nothing -> Left $ MissingField "age"

lookupField :: String -> Header -> CSVRow -> Maybe String
lookupField fieldName header row = 
  case lookup fieldName (zip header row) of
    Just value -> Just value
    Nothing -> Nothing

formatErrors :: [Either ValidationError CSVRow] -> String
formatErrors results = intercalate "\n" $ map formatResult (zip [1..] results)
  where
    formatResult (n, Left err) = "Row " ++ show n ++ ": " ++ show err
    formatResult (_, Right _) = ""
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processEvenSquares

main :: IO ()
main = do
    let testData = [1..10]
    putStrLn $ "Original data: " ++ show testData
    putStrLn $ "Processed data: " ++ show (processEvenSquares testData)
    putStrLn $ "Sum of processed data: " ++ show (sumProcessedData testData)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)