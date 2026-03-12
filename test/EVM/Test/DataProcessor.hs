module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

import Data.List (isInfixOf)
import Data.Char (toLower)

type Row = [String]
type Header = [String]

parseCSV :: String -> (Header, [Row])
parseCSV content = 
    let lines' = lines content
        header = splitByComma (head lines')
        rows = map splitByComma (tail lines')
    in (header, rows)
  where
    splitByComma = map trim . splitOn ','
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
    splitOn _ [] = []
    splitOn delimiter str =
        let (token, rest) = break (== delimiter) str
        in token : case rest of
            [] -> []
            _ -> splitOn delimiter (tail rest)

filterRows :: (Row -> Bool) -> [Row] -> [Row]
filterRows predicate = filter predicate

containsIgnoreCase :: String -> String -> Bool
containsIgnoreCase substr str = 
    toLowerString substr `isInfixOf` toLowerString str
  where
    toLowerString = map toLower

filterByColumn :: Int -> String -> [Row] -> [Row]
filterByColumn colIndex searchTerm = 
    filter (\row -> 
        if colIndex < length row 
        then containsIgnoreCase searchTerm (row !! colIndex)
        else False)

calculateColumnAverage :: Int -> [Row] -> Maybe Double
calculateColumnAverage colIndex rows = 
    let numericValues = mapMaybe (safeRead . (!! colIndex)) rows
    in if null numericValues 
       then Nothing 
       else Just (sum numericValues / fromIntegral (length numericValues))
  where
    safeRead s = case reads s of
        [(x, "")] -> Just x
        _ -> Nothing

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr (\x acc -> case f x of
    Just y -> y : acc
    Nothing -> acc) []module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -100) xs then Just xs else Nothing

main :: IO ()
main = do
    let sampleData = [1, -5, 3, 0, 8, -2]
    case validateInput sampleData of
        Just validData -> do
            putStrLn $ "Original: " ++ show validData
            putStrLn $ "Processed: " ++ show (processNumbers validData)
            putStrLn $ "Sum: " ++ show (sumProcessed validData)
        Nothing -> putStrLn "Input contains invalid numbers"module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (* 2)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

sumProcessed :: [Int] -> Int
sumProcessed = sum . processEvenSquares

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)
    putStrLn $ "First element: " ++ show (safeHead numbers)module DataProcessor where

import Data.List.Split (splitOn)

type Record = (String, [Double])

parseCSV :: String -> [Record]
parseCSV content = map parseLine (lines content)
  where
    parseLine line = 
        let parts = splitOn "," line
            name = head parts
            values = map read (tail parts)
        in (name, values)

computeAverages :: [Record] -> [(String, Double)]
computeAverages records = map avg records
  where
    avg (name, vals) = 
        let total = sum vals
            count = fromIntegral (length vals)
        in (name, total / count)

processData :: String -> [(String, Double)]
processData = computeAverages . parseCSVmodule DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | length xs < n = []
    | otherwise = avg : movingAverage n (tail xs)
  where
    window = take n xs
    avg = sum window / fromIntegral n

-- Example usage with a helper function
exampleData :: [Double]
exampleData = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]

main :: IO ()
main = do
    let result = movingAverage 3 exampleData
    putStrLn "Moving average with window size 3:"
    print result