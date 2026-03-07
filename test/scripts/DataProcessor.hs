module DataProcessor where

import Data.List (tails)

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | n > length xs = error "Window size exceeds list length"
    | otherwise = map average $ filter (\w -> length w == n) $ tails xs
  where
    average ws = sum ws / fromIntegral n

smoothData :: Int -> [Double] -> [Double]
smoothData windowSize dataPoints = 
    movingAverage windowSize dataPoints ++ 
    replicate (windowSize - 1) (last dataPoints)

validateData :: [Double] -> Maybe [Double]
validateData [] = Nothing
validateData xs
    | any isNaN xs = Nothing
    | any isInfinite xs = Nothing
    | otherwise = Just xs

processDataStream :: Int -> [Double] -> Maybe [Double]
processDataStream windowSize rawData = do
    validData <- validateData rawData
    return $ smoothData windowSize validDatamodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processDatamodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x
module DataProcessor where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)

-- | Safely parse an integer from a string, returning Nothing on failure
safeParseInt :: String -> Maybe Int
safeParseInt s = case reads s of
    [(n, "")] -> Just n
    _         -> Nothing

-- | Validate that a string contains only digits
validateDigits :: String -> Bool
validateDigits = all isDigit

-- | Trim leading and trailing whitespace
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

-- | Parse a comma-separated list of integers
parseIntList :: String -> Maybe [Int]
parseIntList input = 
    let trimmed = trim input
        parts = splitOn ',' trimmed
        parsed = map safeParseInt parts
    in if all isJust parsed
        then Just (catMaybes parsed)
        else Nothing
    where
        splitOn :: Char -> String -> [String]
        splitOn delimiter = foldr splitHelper [""]
            where
                splitHelper char acc@(current:rest)
                    | char == delimiter = "":acc
                    | otherwise = (char:current):rest

        isJust :: Maybe a -> Bool
        isJust (Just _) = True
        isJust Nothing  = False

-- | Transform a list of integers by applying a function and filtering
transformData :: (Int -> Int) -> (Int -> Bool) -> [Int] -> [Int]
transformData f predicate = map f . filter predicate

-- | Calculate statistics from a list of integers
data Stats = Stats
    { sumTotal :: Int
    , average  :: Double
    , minVal   :: Int
    , maxVal   :: Int
    } deriving (Show, Eq)

calculateStats :: [Int] -> Maybe Stats
calculateStats [] = Nothing
calculateStats xs = Just Stats
    { sumTotal = sum xs
    , average  = fromIntegral (sum xs) / fromIntegral (length xs)
    , minVal   = minimum xs
    , maxVal   = maxVal
    }
    where maxVal = maximum xs

-- | Format a list of integers as a string with custom separator
formatList :: String -> [Int] -> String
formatList separator = intercalate separator . map show

-- | Main processing pipeline
processData :: String -> Either String Stats
processData input = case parseIntList input of
    Nothing -> Left "Invalid input format"
    Just numbers -> case calculateStats numbers of
        Nothing -> Left "Empty data after parsing"
        Just stats -> Right statsmodule DataProcessor where

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

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows :: Int -> [a] -> [[a]]
    windows m = takeWhile ((== m) . length) . map (take m) . tails
    
    average :: Fractional a => [a] -> a
    average ys = sum ys / fromIntegral (length ys)

-- Helper function from Data.List
tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_:ys) = xs : tails ys