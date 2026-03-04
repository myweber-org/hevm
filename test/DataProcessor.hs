
module DataProcessor where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)

-- | Validates if a string contains only digits
validateDigits :: String -> Bool
validateDigits = all isDigit

-- | Safely parses an integer from string, returns Nothing on failure
safeParseInt :: String -> Maybe Int
safeParseInt str
    | validateDigits str = Just (read str)
    | otherwise = Nothing

-- | Normalizes phone number by removing spaces and dashes
normalizePhone :: String -> String
normalizePhone = filter (\c -> isDigit c || c == '+')

-- | Validates email format (basic check)
validateEmail :: String -> Bool
validateEmail email =
    let parts = split '@' email
    in length parts == 2 && 
       not (null (head parts)) && 
       '.' `elem` (last parts)
  where
    split delimiter = foldr (\c acc -> 
        if c == delimiter 
        then []:acc 
        else (c:head acc):tail acc) [[]]

-- | Processes a list of raw strings into validated integers
processNumbers :: [String] -> [Int]
processNumbers = catMaybes . map safeParseInt

-- | Formats a list of integers as a comma-separated string
formatNumbers :: [Int] -> String
formatNumbers = intercalate ", " . map show

-- | Main processing pipeline example
processData :: [String] -> String
processData inputs = 
    let validated = processNumbers inputs
        total = sum validated
        count = length validated
    in "Processed " ++ show count ++ 
       " numbers totaling " ++ show total

-- | Type-safe configuration parser
data Config = Config 
    { maxRetries :: Int
    , timeout :: Int
    , enabled :: Bool
    } deriving (Show, Eq)

parseConfig :: [(String, String)] -> Config
parseConfig pairs = Config
    { maxRetries = fromMaybe 3 (lookup "maxRetries" pairs >>= safeParseInt)
    , timeout = fromMaybe 30 (lookup "timeout" pairs >>= safeParseInt)
    , enabled = fromMaybe True (fmap read (lookup "enabled" pairs))
    }