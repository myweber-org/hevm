module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData xs = all (> 0) xs && length xs > 3

combineResults :: [Int] -> [Int] -> [Int]
combineResults xs ys = zipWith (+) (processData xs) (processData ys)
module DataProcessor where

import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)
import Text.Read (readMaybe)

-- Type for validated person data
data Person = Person
  { personId :: Int
  , name :: String
  , age :: Int
  , email :: String
  } deriving (Show, Eq)

-- Safely parse integer from string
safeParseInt :: String -> Maybe Int
safeParseInt str
  | all isDigit str = readMaybe str
  | otherwise = Nothing

-- Validate email format (simplified)
validateEmail :: String -> Bool
validateEmail emailStr =
  let parts = splitOn '@' emailStr
   in length parts == 2 && not (null (head parts)) && not (null (last parts))
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr (\c acc -> if c == delimiter then [] : acc else combine c acc) [[]]
      where
        combine :: Char -> [String] -> [String]
        combine x (y : ys) = (x : y) : ys
        combine x [] = [[x]]

-- Parse person from raw string data
parsePerson :: [String] -> Maybe Person
parsePerson [idStr, nameStr, ageStr, emailStr] = do
  personIdVal <- safeParseInt idStr
  ageVal <- safeParseInt ageStr
  guard (ageVal >= 0 && ageVal <= 150)
  guard (validateEmail emailStr)
  guard (not (null nameStr))
  return $ Person personIdVal nameStr ageVal emailStr
parsePerson _ = Nothing

-- Process multiple person records
processRecords :: [[String]] -> [Person]
processRecords = catMaybes . map parsePerson

-- Format person as CSV
formatPersonCSV :: Person -> String
formatPersonCSV person =
  intercalate "," [show (personId person), name person, show (age person), email person]

-- Calculate average age
averageAge :: [Person] -> Double
averageAge persons =
  if null persons
    then 0.0
    else fromIntegral (sum (map age persons)) / fromIntegral (length persons)

-- Filter adults (age >= 18)
filterAdults :: [Person] -> [Person]
filterAdults = filter (\p -> age p >= 18)

-- Main processing pipeline example
processDataPipeline :: [[String]] -> (Double, [String])
processDataPipeline rawData =
  let persons = processRecords rawData
      avg = averageAge persons
      adults = filterAdults persons
      adultCSVs = map formatPersonCSV adults
   in (avg, adultCSVs)

guard :: Bool -> Maybe ()
guard True = Just ()
guard False = Nothing