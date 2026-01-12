
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (\x -> x >= -100 && x <= 100) xs
                   then Just xs
                   else Nothing

main :: IO ()
main = do
    let sampleData = [-5, 2, 0, 8, -3, 10]
    case validateInput sampleData of
        Just validData -> do
            let result = processData validData
            putStrLn $ "Original: " ++ show validData
            putStrLn $ "Processed: " ++ show result
        Nothing -> putStrLn "Input validation failed: values out of range"module DataProcessor where

import Data.List (sort, group)
import Data.Maybe (catMaybes)

type Record = (String, Int, Double)

parseCSVLine :: String -> Maybe Record
parseCSVLine line = case words line of
    [name, ageStr, salaryStr] -> 
        case (reads ageStr, reads salaryStr) of
            ([(age, "")], [(salary, "")]) -> Just (name, age, salary)
            _ -> Nothing
    _ -> Nothing

loadCSVData :: String -> IO [Record]
loadCSVData filename = do
    content <- readFile filename
    return $ catMaybes $ map parseCSVLine (lines content)

calculateAverageSalary :: [Record] -> Double
calculateAverageSalary records = 
    let salaries = map (\(_, _, salary) -> salary) records
    in if null salaries then 0.0 else sum salaries / fromIntegral (length salaries)

groupByAgeRange :: [Record] -> [(String, Int)]
groupByAgeRange records = 
    let ageGroups = group $ sort $ map (\(_, age, _) -> age `div` 10 * 10) records
    in map (\g -> (show (head g) ++ "-" ++ show (head g + 9), length g)) ageGroups

filterHighEarners :: [Record] -> Double -> [Record]
filterHighEarners records threshold = 
    filter (\(_, _, salary) -> salary > threshold) records

processDataPipeline :: String -> Double -> IO ()
processDataPipeline filename threshold = do
    records <- loadCSVData filename
    putStrLn $ "Total records: " ++ show (length records)
    putStrLn $ "Average salary: " ++ show (calculateAverageSalary records)
    putStrLn "Age distribution:"
    mapM_ (\(range, count) -> putStrLn $ "  " ++ range ++ ": " ++ show count) 
          (groupByAgeRange records)
    putStrLn $ "High earners (> " ++ show threshold ++ "): " 
               ++ show (length $ filterHighEarners records threshold)