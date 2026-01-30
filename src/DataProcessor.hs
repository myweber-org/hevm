module DataProcessor where

import Data.Char (isDigit, isAlpha, toUpper)

validateEmail :: String -> Bool
validateEmail email = '@' `elem` email && '.' `elem` afterAt
  where afterAt = dropWhile (/= '@') email

validatePhone :: String -> Bool
validatePhone = all isDigit

normalizeName :: String -> String
normalizeName = unwords . map capitalize . words
  where capitalize [] = []
        capitalize (x:xs) = toUpper x : map toLower xs

sanitizeInput :: String -> String
sanitizeInput = filter (\c -> isAlpha c || isDigit c || c `elem` " -_@.")

processUserData :: String -> String -> String -> Maybe (String, String, String)
processUserData name email phone
  | not (validateEmail email) = Nothing
  | not (validatePhone phone) = Nothing
  | otherwise = Just (normalizeName name, sanitizeInput email, phone)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map avg $ windows n xs
  where
    windows :: Int -> [a] -> [[a]]
    windows m = takeWhile ((== m) . length) . map (take m) . iterate tail
    
    avg :: [Double] -> Double
    avg ys = sum ys / fromIntegral (length ys)

-- Example usage
sampleData :: [Double]
sampleData = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]

main :: IO ()
main = do
    putStrLn "Original data:"
    print sampleData
    putStrLn "\nMoving average with window size 3:"
    print $ movingAverage 3 sampleData
    putStrLn "\nMoving average with window size 5:"
    print $ movingAverage 5 sampleData