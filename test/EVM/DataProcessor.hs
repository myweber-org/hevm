
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)
module DataProcessor where

import Data.Char (toLower, isAlpha, isSpace)
import Data.List (intercalate)

data UserProfile = UserProfile
  { userName :: String
  , userAge :: Int
  , userEmail :: String
  } deriving (Show, Eq)

validateUserName :: String -> Maybe String
validateUserName name
  | all (\c -> isAlpha c || isSpace c) name && length name >= 2 = Just name
  | otherwise = Nothing

validateUserAge :: Int -> Maybe Int
validateUserAge age
  | age >= 0 && age <= 150 = Just age
  | otherwise = Nothing

validateUserEmail :: String -> Maybe String
validateUserEmail email
  | '@' `elem` email && '.' `elem` (dropWhile (/= '@') email) = Just email
  | otherwise = Nothing

createUserProfile :: String -> Int -> String -> Maybe UserProfile
createUserProfile name age email = do
  validName <- validateUserName name
  validAge <- validateUserAge age
  validEmail <- validateUserEmail email
  return $ UserProfile validName validAge validEmail

normalizeUserName :: String -> String
normalizeUserName = unwords . map capitalize . words
  where
    capitalize [] = []
    capitalize (x:xs) = toUpper x : map toLower xs
    toUpper = toEnum . subtract 32 . fromEnum

formatProfileOutput :: UserProfile -> String
formatProfileOutput profile =
  intercalate "\n"
    [ "User Profile:"
    , "  Name: " ++ userName profile
    , "  Age: " ++ show (userAge profile)
    , "  Email: " ++ userEmail profile
    ]

processUserData :: [(String, Int, String)] -> [Maybe UserProfile]
processUserData = map (\(n, a, e) -> createUserProfile n a e)

filterValidProfiles :: [Maybe UserProfile] -> [UserProfile]
filterValidProfiles = foldr maybeCons []
  where
    maybeCons (Just x) xs = x : xs
    maybeCons Nothing xs = xs

calculateAverageAge :: [UserProfile] -> Double
calculateAverageAge profiles =
  if null profiles
    then 0.0
    else fromIntegral (sum (map userAge profiles)) / fromIntegral (length profiles)