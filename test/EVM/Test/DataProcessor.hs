module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x
module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isAlpha)

data ValidationError = InvalidFormat String
                     | MissingField String
                     | InvalidValue String String
                     deriving (Show, Eq)

type CSVRow = [String]
type ValidationResult = Either ValidationError CSVRow

validateCSVRow :: CSVRow -> ValidationResult
validateCSVRow row
    | length row < 3 = Left $ MissingField "Row must contain at least 3 fields"
    | not $ allFieldsNonEmpty row = Left $ InvalidFormat "All fields must be non-empty"
    | not $ isValidId (row !! 0) = Left $ InvalidValue "ID" (row !! 0)
    | not $ isValidName (row !! 1) = Left $ InvalidValue "Name" (row !! 1)
    | not $ isValidAge (row !! 2) = Left $ InvalidValue "Age" (row !! 2)
    | otherwise = Right row
  where
    allFieldsNonEmpty = all (not . null)
    isValidId = all isDigit
    isValidName = all isAlpha
    isValidAge str = all isDigit str && let age = read str in age > 0 && age < 150

parseCSV :: String -> [ValidationResult]
parseCSV = map (validateCSVRow . splitByComma) . lines
  where
    splitByComma = takeWhile (not . null) . unfoldr (Just . break (== ','))

processCSVFile :: String -> IO ()
processCSVFile content = do
    let results = parseCSV content
    mapM_ (either printError printSuccess) (zip [1..] results)
    putStrLn $ "\nSummary: " ++ show (countValid results) ++ " valid, " ++ show (countInvalid results) ++ " invalid"
  where
    printError (lineNum, Left err) = putStrLn $ "Line " ++ show lineNum ++ ": ERROR - " ++ show err
    printSuccess (lineNum, Right row) = putStrLn $ "Line " ++ show lineNum ++ ": OK - " ++ intercalate ", " row
    countValid = length . filter isRight
    countInvalid = length . filter isLeft
    isRight (Right _) = True
    isRight _ = False
    isLeft (Left _) = True
    isLeft _ = False

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f b = case f b of
    Nothing -> []
    Just (a, b') -> a : unfoldr f b'