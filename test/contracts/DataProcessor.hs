module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | length xs < n = []
    | otherwise = avg : movingAverage n (tail xs)
    where
        window = take n xs
        avg = sum window / fromIntegral n

smoothData :: Fractional a => Int -> [a] -> [a]
smoothData windowSize dataPoints =
    let padSize = windowSize `div` 2
        padded = replicate padSize (head dataPoints) ++ dataPoints ++ replicate padSize (last dataPoints)
    in movingAverage windowSize padded

calculateTrend :: (Fractional a, Ord a) => [a] -> [Ordering]
calculateTrend values = zipWith compare (tail values) values
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show result
module DataProcessor where

import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Control.Applicative ((<|>))

-- Type for validated person data
data Person = Person
  { personName :: String
  , personAge  :: Int
  , personId   :: String
  } deriving (Show, Eq)

-- Safe age parser with validation
parseAge :: String -> Maybe Int
parseAge str
  | all isDigit str && not (null str) = 
      let age = read str
      in if age >= 0 && age <= 150 then Just age else Nothing
  | otherwise = Nothing

-- Validate ID format (alphanumeric, 6-12 chars)
validateId :: String -> Maybe String
validateId idStr
  | length idStr >= 6 && length idStr <= 12 &&
    all (\c -> isDigit c || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) idStr =
      Just idStr
  | otherwise = Nothing

-- Parse a person from raw strings
parsePerson :: String -> String -> String -> Maybe Person
parsePerson name ageStr idStr = do
  age <- parseAge ageStr
  validId <- validateId idStr
  if null name 
    then Nothing
    else Just $ Person name age validId

-- Process multiple raw records
processRecords :: [(String, String, String)] -> [Person]
processRecords = catMaybes . map (\(n,a,i) -> parsePerson n a i)

-- Format person for display
formatPerson :: Person -> String
formatPerson p = 
  intercalate " | " 
    [ personName p
    , show (personAge p)
    , personId p
    ]

-- Calculate average age safely
averageAge :: [Person] -> Maybe Double
averageAge [] = Nothing
averageAge persons = 
  let total = sum (map personAge persons)
      count = length persons
  in Just (fromIntegral total / fromIntegral count)

-- Filter adults
filterAdults :: [Person] -> [Person]
filterAdults = filter (\p -> personAge p >= 18)

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
  let rawData = 
        [ ("Alice", "25", "AB123C")
        , ("Bob", "17", "XYZ789")
        , ("Invalid", "twohundred", "ID")
        , ("Carol", "30", "PASS456")
        ]
  
  putStrLn "Processing records..."
  let persons = processRecords rawData
  
  putStrLn "\nValid persons:"
  mapM_ (putStrLn . formatPerson) persons
  
  putStrLn "\nAdults only:"
  mapM_ (putStrLn . formatPerson) (filterAdults persons)
  
  case averageAge persons of
    Just avg -> putStrLn $ "\nAverage age: " ++ show avg
    Nothing -> putStrLn "\nNo valid persons for average calculation"module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print result