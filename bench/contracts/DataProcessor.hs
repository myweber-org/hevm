module DataProcessor where

import Data.List (tails)

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | n > length xs = []
    | otherwise = map average $ filter (\w -> length w == n) $ tails xs
  where
    average ws = sum ws / fromIntegral n

smoothData :: [Double] -> [Double]
smoothData = movingAverage 3

main :: IO ()
main = do
    let sampleData = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
    putStrLn "Original data:"
    print sampleData
    putStrLn "\nSmoothed data (3-point moving average):"
    print $ smoothData sampleDatamodule DataProcessor where

import Data.Char (toUpper)

-- Validate that a string is non-empty and contains only alphabetic characters
validateName :: String -> Maybe String
validateName "" = Nothing
validateName name
    | all isAlpha name = Just name
    | otherwise = Nothing
  where
    isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

-- Convert a string to title case
toTitleCase :: String -> String
toTitleCase = unwords . map capitalize . words
  where
    capitalize [] = []
    capitalize (x:xs) = toUpper x : map toLower xs
    toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

-- Process a list of names: validate, transform to title case, and filter valid ones
processNames :: [String] -> [String]
processNames = map toTitleCase . filterNames
  where
    filterNames = foldr (\name acc -> case validateName name of
        Just validName -> validName : acc
        Nothing -> acc) []

-- Calculate average length of valid names
averageNameLength :: [String] -> Maybe Double
averageNameLength names =
    let validNames = map toTitleCase $ filterNames names
        totalLength = sum $ map length validNames
        count = length validNames
    in if count > 0
        then Just (fromIntegral totalLength / fromIntegral count)
        else Nothing
  where
    filterNames = foldr (\name acc -> case validateName name of
        Just validName -> validName : acc
        Nothing -> acc) []