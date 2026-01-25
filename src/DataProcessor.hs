module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (* 2)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    print result
module DataProcessor where

import Data.List.Split (splitOn)

parseCSV :: String -> [[Double]]
parseCSV csv = map (map read . splitOn ",") $ lines csv

calculateAverages :: [[Double]] -> [Double]
calculateAverages rows = map avg rows
  where
    avg xs = sum xs / fromIntegral (length xs)

processCSVData :: String -> [Double]
processCSVData = calculateAverages . parseCSV

main :: IO ()
main = do
  let csvData = "1.0,2.0,3.0\n4.0,5.0,6.0\n7.0,8.0,9.0"
  let averages = processCSVData csvData
  putStrLn "Averages per row:"
  mapM_ (putStrLn . show) averagesmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData = all (> 0)

main :: IO ()
main = do
    let sampleData = [1, -2, 3, -4, 5]
    putStrLn $ "Original data: " ++ show sampleData
    putStrLn $ "Processed data: " ++ show (processData sampleData)
    putStrLn $ "Data validation: " ++ show (validateData sampleData)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processData
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processEvenSquares

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list (even numbers squared): " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of processed values: " ++ show (sumProcessed numbers)module DataProcessor where

import Data.Char (toUpper)

data Person = Person
  { name :: String
  , age :: Int
  , email :: String
  } deriving (Show, Eq)

validatePerson :: Person -> Either String Person
validatePerson p
  | null (name p) = Left "Name cannot be empty"
  | age p < 0 = Left "Age cannot be negative"
  | age p > 150 = Left "Age cannot exceed 150"
  | not (isValidEmail (email p)) = Left "Invalid email format"
  | otherwise = Right p

isValidEmail :: String -> Bool
isValidEmail emailStr =
  let parts = split '@' emailStr
  in length parts == 2 && 
     not (null (head parts)) && 
     '.' `elem` (last parts)
  where
    split delimiter = foldr (\c acc -> if c == delimiter then []:acc else (c:head acc):tail acc) [[]]

transformPerson :: Person -> Person
transformPerson p = p
  { name = map toUpper (name p)
  , email = map toLower (email p)
  }

processPersonData :: [Person] -> [Either String Person]
processPersonData persons = 
  map (fmap transformPerson . validatePerson) persons

filterValidPersons :: [Either String Person] -> [Person]
filterValidPersons = foldr extract []
  where
    extract (Right p) acc = p : acc
    extract (Left _) acc = acc

calculateAverageAge :: [Person] -> Double
calculateAverageAge persons =
  if null persons 
    then 0.0
    else fromIntegral (sum (map age persons)) / fromIntegral (length persons)