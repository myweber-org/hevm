module DataProcessor where

import Data.List (intercalate)
import Data.Maybe (catMaybes, mapMaybe)
import Text.Read (readMaybe)

data ValidationError = InvalidColumnCount Int Int
                     | InvalidInteger String
                     | InvalidDouble String
                     | RowParseError Int String
                     deriving (Show, Eq)

type Row = [String]
type ValidatedRow = Either ValidationError (Int, Double, String)

parseCSV :: String -> [Row]
parseCSV content = map (splitOn ',') (lines content)
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitHelper [""]
      where
        splitHelper ch (current:rest)
          | ch == delimiter = "":current:rest
          | otherwise = (ch:current):rest

validateRow :: Int -> Row -> ValidatedRow
validateRow rowNum cells
  | length cells /= 3 = Left $ InvalidColumnCount 3 (length cells)
  | otherwise = case (parseInt (cells !! 0), parseDouble (cells !! 1)) of
      (Just idVal, Just scoreVal) -> Right (idVal, scoreVal, cells !! 2)
      (Nothing, _) -> Left $ InvalidInteger (cells !! 0)
      (_, Nothing) -> Left $ InvalidDouble (cells !! 1)
  where
    parseInt :: String -> Maybe Int
    parseInt = readMaybe
    
    parseDouble :: String -> Maybe Double
    parseDouble = readMaybe

processCSVData :: String -> ([ValidatedRow], [ValidationError])
processCSVData content = partitionResults $ zipWith validateRow [1..] rows
  where
    rows = parseCSV content
    partitionResults = foldr splitResult ([], [])
    splitResult row (valids, errs) = case row of
      Left err -> (valids, err:errs)
      Right val -> (row:valids, errs)

formatResults :: ([ValidatedRow], [ValidationError]) -> String
formatResults (validRows, errors) = 
  "Valid rows: " ++ show (length validRows) ++ "\n" ++
  "Errors: " ++ show (length errors) ++ "\n" ++
  if null errors then "" else "Error details:\n" ++ formatErrors errors
  where
    formatErrors = intercalate "\n" . map show

safeReadCSV :: FilePath -> IO (Either String ([ValidatedRow], [ValidationError]))
safeReadCSV path = do
  content <- readFile path
  return $ Right $ processCSVData content