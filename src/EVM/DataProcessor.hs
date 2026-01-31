module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print result
module DataProcessor where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)

-- | Safely parse an integer from a string
safeParseInt :: String -> Maybe Int
safeParseInt str = case reads str of
    [(n, "")] -> Just n
    _         -> Nothing

-- | Validate email format (simplified)
validateEmail :: String -> Bool
validateEmail email =
    let parts = splitOn '@' email
    in length parts == 2 && 
       not (null (head parts)) && 
       '.' `elem` (last parts)

-- | Split string on delimiter
splitOn :: Char -> String -> [String]
splitOn delim = foldr f [[]]
    where f c (x:xs) | c == delim = []:x:xs
                     | otherwise = (c:x):xs

-- | Trim whitespace from both ends
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

-- | Transform list of strings to CSV format
toCSV :: [String] -> String
toCSV = intercalate "," . map escapeCSV
    where escapeCSV s | ',' `elem` s || '"' `elem` s = "\"" ++ replace '"' "\"\"" s ++ "\""
                      | otherwise = s
          replace old new = intercalate new . splitOn old

-- | Process user input data
processUserData :: String -> Either String [Int]
processUserData input =
    let numbers = map trim $ splitOn ',' input
        parsed = map safeParseInt numbers
    in if all isJust parsed
        then Right $ catMaybes parsed
        else Left "Invalid number in input"
    where isJust (Just _) = True
          isJust Nothing  = False

-- | Calculate statistics from numeric data
calculateStats :: [Int] -> (Double, Int, Int)
calculateStats [] = (0.0, 0, 0)
calculateStats nums =
    let total = fromIntegral (sum nums)
        count = length nums
        avg = total / fromIntegral count
        minVal = minimum nums
        maxVal = maximum nums
    in (avg, minVal, maxVal)