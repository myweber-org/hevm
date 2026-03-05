
module DataProcessor where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)
import Control.Applicative ((<|>))

-- | Represents a validated person record
data Person = Person
  { personName :: String
  , personAge  :: Int
  , personEmail :: Maybe String
  } deriving (Show, Eq)

-- | Validation result type
type Validation a = Either String a

-- | Parse a string into a Person with validation
parsePerson :: String -> String -> String -> Validation Person
parsePerson nameStr ageStr emailStr = do
  name <- validateName nameStr
  age <- validateAge ageStr
  email <- validateEmail emailStr
  pure $ Person name age email

-- | Validate name (non-empty, no digits)
validateName :: String -> Validation String
validateName "" = Left "Name cannot be empty"
validateName name
  | any isDigit name = Left "Name cannot contain digits"
  | otherwise = Right (trim name)

-- | Validate age (positive integer, reasonable range)
validateAge :: String -> Validation Int
validateAge str = case reads (trim str) of
  [(age, "")] -> 
    if age >= 0 && age <= 150
      then Right age
      else Left "Age must be between 0 and 150"
  _ -> Left "Age must be a valid integer"

-- | Validate email (optional, but must contain @ if present)
validateEmail :: String -> Validation (Maybe String)
validateEmail "" = Right Nothing
validateEmail email
  | '@' `elem` trimmed = Right (Just trimmed)
  | otherwise = Left "Email must contain @ symbol"
  where trimmed = trim email

-- | Trim whitespace from both ends
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- | Process a list of raw person data
processPersonList :: [(String, String, String)] -> [Person]
processPersonList = catMaybes . map processSingle
  where
    processSingle (name, age, email) =
      case parsePerson name age email of
        Right person -> Just person
        Left err -> Nothing

-- | Safe string to int conversion
safeReadInt :: String -> Maybe Int
safeReadInt str = case reads (trim str) of
  [(n, "")] -> Just n
  _ -> Nothing

-- | Format person for display
formatPerson :: Person -> String
formatPerson person = intercalate " | "
  [ personName person
  , show (personAge person)
  , fromMaybe "N/A" (personEmail person)
  ]

-- | Example usage
exampleUsage :: IO ()
exampleUsage = do
  let rawData = 
        [ ("John Doe", "30", "john@example.com")
        , ("Jane Smith", "25", "")
        , ("Invalid123", "200", "bad-email")
        ]
  
  putStrLn "Processing person data:"
  mapM_ (putStrLn . formatPerson) (processPersonList rawData)
  
  putStrLn "\nIndividual validations:"
  print $ parsePerson "Alice" "28" "alice@test.org"
  print $ parsePerson "Bob" "invalid" "bob@test.org"module DataProcessor where

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows :: Int -> [a] -> [[a]]
    windows m = takeWhile ((== m) . length) . map (take m) . tails
    
    average :: [Double] -> Double
    average ys = sum ys / fromIntegral (length ys)

smoothData :: [Double] -> [Double]
smoothData = movingAverage 3

calculateTrend :: [Double] -> Maybe Double
calculateTrend [] = Nothing
calculateTrend [_] = Nothing
calculateTrend values = Just slope
  where
    n = fromIntegral $ length values
    indices = [0..n-1]
    sumX = sum indices
    sumY = sum values
    sumXY = sum $ zipWith (*) indices values
    sumX2 = sum $ map (^2) indices
    slope = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX)