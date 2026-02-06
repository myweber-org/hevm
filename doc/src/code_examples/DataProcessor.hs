module DataProcessor where

import Data.List (intercalate)
import Text.Read (readMaybe)

type Row = [String]
type Table = [Row]

parseCSV :: String -> Table
parseCSV content = map (splitOn ',') (lines content)
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitHelper [""]
      where
        splitHelper char acc@(current:rest)
          | char == delimiter = "":acc
          | otherwise = (char:current):rest

filterRows :: (Row -> Bool) -> Table -> Table
filterRows predicate = filter predicate

numericGreaterThan :: Row -> Int -> String -> Bool
numericGreaterThan row columnIndex thresholdStr =
  case (getColumn row columnIndex, readMaybe thresholdStr) of
    (Just cell, Just threshold) ->
      case readMaybe cell of
        Just value -> value > threshold
        Nothing -> False
    _ -> False
  where
    getColumn :: Row -> Int -> Maybe String
    getColumn row idx
      | idx >= 0 && idx < length row = Just (row !! idx)
      | otherwise = Nothing

processData :: String -> String -> String -> Either String Table
processData content columnStr thresholdStr = do
  table <- Right $ parseCSV content
  columnIndex <- case readMaybe columnStr of
    Just idx | idx >= 0 -> Right idx
    _ -> Left "Invalid column index"
  
  let filtered = filterRows (\row -> numericGreaterThan row columnIndex thresholdStr) table
  return filtered

formatTable :: Table -> String
formatTable = intercalate "\n" . map (intercalate ",")

main :: IO ()
main = do
  let sampleData = "id,name,score\n1,Alice,85\n2,Bob,72\n3,Charlie,93\n4,Diana,68"
  case processData sampleData "2" "80" of
    Left err -> putStrLn $ "Error: " ++ err
    Right result -> putStrLn $ "Filtered results:\n" ++ formatTable result