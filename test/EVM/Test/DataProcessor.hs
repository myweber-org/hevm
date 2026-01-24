
module DataProcessor where

import Data.Char (toLower, isAlpha, isSpace)
import Data.List (intercalate)
import Data.Maybe (catMaybes)

type Username = String
type Email = String
type Age = Int

data UserProfile = UserProfile
  { username :: Username
  , email :: Email
  , age :: Age
  } deriving (Show, Eq)

normalizeUsername :: Username -> Maybe Username
normalizeUsername name
  | length trimmed < 3 = Nothing
  | any (not . isValidChar) trimmed = Nothing
  | otherwise = Just (map toLower trimmed)
  where
    trimmed = trim name
    isValidChar c = isAlpha c || c == '_' || c == '-'

validateEmail :: Email -> Maybe Email
validateEmail emailStr
  | '@' `elem` emailStr && '.' `elem` (dropWhile (/= '@') emailStr) = Just emailStr
  | otherwise = Nothing

validateAge :: Age -> Maybe Age
validateAge a
  | a >= 0 && a <= 150 = Just a
  | otherwise = Nothing

createUserProfile :: Username -> Email -> Age -> Maybe UserProfile
createUserProfile name emailStr ageVal = do
  normName <- normalizeUsername name
  validEmail <- validateEmail emailStr
  validAge <- validateAge ageVal
  return $ UserProfile normName validEmail validAge

processUserList :: [(Username, Email, Age)] -> [UserProfile]
processUserList = catMaybes . map (\(n, e, a) -> createUserProfile n e a)

formatProfile :: UserProfile -> String
formatProfile (UserProfile name email age) =
  intercalate " | " [name, email, show age]

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

main :: IO ()
main = do
  let rawUsers = 
        [ ("john_doe", "john@example.com", 25)
        , ("alice-smith", "alice@test.org", 30)
        , ("ab", "invalid", 200)
        , ("bob123", "bob@domain.net", 45)
        ]
  
  putStrLn "Valid user profiles:"
  mapM_ (putStrLn . formatProfile) (processUserList rawUsers)