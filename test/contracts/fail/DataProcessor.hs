module DataProcessor where

import Data.Char (isDigit, isAlpha, toUpper)
import Data.List (intercalate)

-- Validate that a string contains only alphanumeric characters
validateAlphanumeric :: String -> Bool
validateAlphanumeric = all (\c -> isAlpha c || isDigit c)

-- Transform a string to uppercase and reverse it
transformString :: String -> String
transformString = reverse . map toUpper

-- Process a list of strings, validating and transforming each
processData :: [String] -> [Maybe String]
processData = map processSingle
  where
    processSingle str
      | validateAlphanumeric str = Just (transformString str)
      | otherwise = Nothing

-- Filter out failed validations and join successful results
formatResults :: [Maybe String] -> String
formatResults results = 
  let successes = [s | Just s <- results]
  in intercalate ", " successes

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
  let inputData = ["hello123", "test456", "invalid!"]
  let processed = processData inputData
  putStrLn $ "Processed results: " ++ formatResults processed
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessed :: (Int -> Int) -> [Int] -> Int
sumProcessed processor = sum . map processor

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn "Original list:"
    print numbers
    
    putStrLn "\nSquares of even numbers:"
    let squares = processEvenSquares numbers
    print squares
    
    putStrLn "\nSum of doubled numbers:"
    let total = sumProcessed (*2) numbers
    print total
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers