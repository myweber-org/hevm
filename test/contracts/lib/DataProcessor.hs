module DataProcessor where

import Data.Char (toLower, isAlpha, isSpace)
import Data.List (intercalate)

type Username = String
type Email = String
type UserProfile = (Username, Email, Int)

validateUsername :: Username -> Maybe Username
validateUsername username
    | length username < 3 = Nothing
    | length username > 20 = Nothing
    | not (all isAlpha username) = Nothing
    | otherwise = Just (map toLower username)

validateEmail :: Email -> Maybe Email
validateEmail email
    | '@' `notElem` email = Nothing
    | '.' `notElem` (dropWhile (/= '@') email) = Nothing
    | any isSpace email = Nothing
    | otherwise = Just (map toLower email)

normalizeProfile :: UserProfile -> Maybe UserProfile
normalizeProfile (username, email, age) = do
    validUsername <- validateUsername username
    validEmail <- validateEmail email
    if age >= 0 && age <= 150
        then Just (validUsername, validEmail, age)
        else Nothing

formatProfile :: UserProfile -> String
formatProfile (username, email, age) =
    intercalate " | " ["Username: " ++ username, "Email: " ++ email, "Age: " ++ show age]

processProfiles :: [UserProfile] -> [String]
processProfiles profiles =
    map formatProfile $ filter (/= Nothing) (map normalizeProfile profiles) >>= maybeToList
  where
    maybeToList (Just x) = [x]
    maybeToList Nothing = []