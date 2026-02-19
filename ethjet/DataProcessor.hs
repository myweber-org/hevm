module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-5, 3, 0, 8, -2, 10]
    let result = processNumbers numbers
    print result
module DataProcessor where

import Data.Time
import Data.Time.Format
import Data.List
import System.Locale

type Record = (Day, String, Double)

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

parseRecord :: String -> Maybe Record
parseRecord line = case words line of
  [dateStr, name, valueStr] -> do
    date <- parseDate dateStr
    value <- readMaybe valueStr
    return (date, name, value)
  _ -> Nothing
  where readMaybe s = case reads s of
          [(x, "")] -> Just x
          _ -> Nothing

filterByDateRange :: Day -> Day -> [Record] -> [Record]
filterByDateRange start end = filter (\(d, _, _) -> d >= start && d <= end)

loadRecords :: FilePath -> IO [Record]
loadRecords path = do
  content <- readFile path
  return $ catMaybes $ map parseRecord $ lines content

summarizeRecords :: [Record] -> [(String, Double)]
summarizeRecords records =
  let grouped = groupBy (\(_, n1, _) (_, n2, _) -> n1 == n2) $
                sortBy (\(_, n1, _) (_, n2, _) -> compare n1 n2) records
  in map (\g -> let (_, name, _) = head g
                    total = sum $ map (\(_, _, v) -> v) g
                in (name, total)) grouped

processDataFile :: FilePath -> Day -> Day -> IO ()
processDataFile path start end = do
  records <- loadRecords path
  let filtered = filterByDateRange start end records
  let summary = summarizeRecords filtered
  putStrLn "Summary of filtered records:"
  mapM_ (\(name, total) -> putStrLn $ name ++ ": " ++ show total) summarymodule DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
    where
        windows :: Int -> [a] -> [[a]]
        windows size list = case splitAt size list of
            (window, rest) -> if length window == size
                then window : windows size (tail list)
                else []
        
        average :: Fractional a => [a] -> a
        average vals = sum vals / fromIntegral (length vals)