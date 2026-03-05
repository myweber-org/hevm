module DataProcessor where

import Data.List (tails)

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map avg $ filter (\window -> length window == n) $ tails xs
  where
    avg window = sum window / fromIntegral n

smoothData :: Fractional a => Int -> [a] -> [a]
smoothData windowSize dataPoints =
    movingAverage windowSize dataPoints

calculateTrend :: Fractional a => [a] -> a
calculateTrend values =
    let n = fromIntegral $ length values
        indices = [0..n-1]
        sumX = sum indices
        sumY = sum values
        sumXY = sum $ zipWith (*) indices values
        sumX2 = sum $ map (^2) indices
        slope = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX)
    in slope
module DataProcessor where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Record = (String, Double, Double)

parseCSV :: String -> [Record]
parseCSV content = mapMaybe parseLine (lines content)
  where
    parseLine line = case splitOn "," line of
      [name, val1Str, val2Str] -> 
        case (reads val1Str, reads val2Str) of
          ([(val1, "")], [(val2, "")]) -> Just (name, val1, val2)
          _ -> Nothing
      _ -> Nothing

calculateAverages :: [Record] -> (Double, Double)
calculateAverages records = (avg val1s, avg val2s)
  where
    (val1s, val2s) = unzip [(v1, v2) | (_, v1, v2) <- records]
    avg xs = sum xs / fromIntegral (length xs)

processData :: String -> (Double, Double)
processData = calculateAverages . parseCSVmodule DataProcessor where

import Data.Char (toLower, isAlpha, isSpace)
import Data.List (intercalate)

type Username = String
type Email = String
type Age = Int

data UserProfile = UserProfile
    { username :: Username
    , email :: Email
    , age :: Age
    } deriving (Show, Eq)

validateUsername :: Username -> Either String Username
validateUsername name
    | null name = Left "Username cannot be empty"
    | length name < 3 = Left "Username must be at least 3 characters"
    | length name > 20 = Left "Username cannot exceed 20 characters"
    | not (all isAlpha name) = Left "Username must contain only letters"
    | otherwise = Right (map toLower name)

validateEmail :: Email -> Either String Email
validateEmail email
    | '@' `notElem` email = Left "Email must contain @ symbol"
    | '.' `notElem` (dropWhile (/= '@') email) = Left "Email must contain domain with dot"
    | any isSpace email = Left "Email cannot contain spaces"
    | otherwise = Right (map toLower email)

validateAge :: Age -> Either String Age
validateAge age
    | age < 0 = Left "Age cannot be negative"
    | age > 150 = Left "Age must be realistic (<= 150)"
    | age < 13 = Left "User must be at least 13 years old"
    | otherwise = Right age

createUserProfile :: Username -> Email -> Age -> Either String UserProfile
createUserProfile uname mail userAge = do
    validName <- validateUsername uname
    validEmail <- validateEmail mail
    validAge <- validateAge userAge
    return $ UserProfile validName validEmail validAge

formatProfile :: UserProfile -> String
formatProfile (UserProfile name email age) =
    intercalate "\n"
        [ "Username: " ++ name
        , "Email: " ++ email
        , "Age: " ++ show age
        ]

sanitizeInput :: String -> String
sanitizeInput = unwords . words . filter (/= '\t')

processUserData :: [String] -> [Either String UserProfile]
processUserData inputs = map processSingle inputs
  where
    processSingle input = case words (sanitizeInput input) of
        [name, email, ageStr] ->
            case reads ageStr of
                [(age, "")] -> createUserProfile name email age
                _ -> Left "Invalid age format"
        _ -> Left "Invalid input format: expected 'username email age'"