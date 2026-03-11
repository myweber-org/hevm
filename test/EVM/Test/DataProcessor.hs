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
    Nothing -> acc) []