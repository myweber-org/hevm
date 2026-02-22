
module DataProcessor where

import Data.Char (isAlpha, isDigit, toLower)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)

data UserProfile = UserProfile
  { username :: String
  , email :: String
  , age :: Int
  , tags :: [String]
  } deriving (Show, Eq)

validateUsername :: String -> Maybe String
validateUsername name
  | length name >= 3 && length name <= 20 && all isAlpha name = Just name
  | otherwise = Nothing

validateEmail :: String -> Maybe String
validateEmail emailStr
  | '@' `elem` emailStr && '.' `elem` (dropWhile (/= '@') emailStr) = Just emailStr
  | otherwise = Nothing

validateAge :: Int -> Maybe Int
validateAge a
  | a >= 0 && a <= 150 = Just a
  | otherwise = Nothing

normalizeTags :: [String] -> [String]
normalizeTags = map (map toLower) . filter (not . null)

createUserProfile :: String -> String -> Int -> [String] -> Maybe UserProfile
createUserProfile name emailStr ageVal tagsList = do
  validName <- validateUsername name
  validEmail <- validateEmail emailStr
  validAge <- validateAge ageVal
  let normalizedTags = normalizeTags tagsList
  return $ UserProfile validName validEmail validAge normalizedTags

parseUserFromCSV :: String -> Maybe UserProfile
parseUserFromCSV csvLine = case splitOn ',' csvLine of
  [name, emailStr, ageStr, tagsStr] -> do
    ageVal <- readMaybe ageStr
    let tags = splitOn ';' tagsStr
    createUserProfile name emailStr ageVal tags
  _ -> Nothing
  where
    splitOn delimiter = foldr splitHelper [""]
      where
        splitHelper char acc
          | char == delimiter = "" : acc
          | otherwise = (char : head acc) : tail acc

profileSummary :: UserProfile -> String
profileSummary (UserProfile name emailStr ageVal tags) =
  intercalate " | "
    [ "User: " ++ name
    , "Email: " ++ emailStr
    , "Age: " ++ show ageVal
    , "Tags: " ++ intercalate ", " tags
    ]

processUserBatch :: [String] -> [UserProfile]
processUserBatch = catMaybes . map parseUserFromCSV

averageAge :: [UserProfile] -> Double
averageAge profiles =
  if null profiles
    then 0.0
    else fromIntegral (sum (map age profiles)) / fromIntegral (length profiles)

filterByTag :: String -> [UserProfile] -> [UserProfile]
filterByTag tag = filter (\p -> map toLower tag `elem` tags p)