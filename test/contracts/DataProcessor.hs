module DataProcessor where

import Data.Char (isAlpha, isDigit, toLower)
import Data.List (intercalate)

type Username = String
type Email = String
type UserData = (Username, Email)

validateUsername :: Username -> Bool
validateUsername name = 
    let len = length name
        validChars = all (\c -> isAlpha c || isDigit c || c `elem` "_-") name
    in len >= 3 && len <= 20 && validChars

validateEmail :: Email -> Bool
validateEmail email =
    let parts = splitOn '@' email
        hasAt = length parts == 2
        [local, domain] = if hasAt then parts else ["", ""]
        validLocal = not (null local) && all (\c -> isAlpha c || isDigit c || c `elem` "._%+-") local
        validDomain = '.' `elem` domain && all (\c -> isAlpha c || isDigit c || c `elem` ".-") domain
    in hasAt && validLocal && validDomain

normalizeUsername :: Username -> Username
normalizeUsername = map toLower . filter (/= ' ')

formatUserDisplay :: UserData -> String
formatUserDisplay (name, email) =
    intercalate " | " ["User: " ++ name, "Email: " ++ email]

processUserInput :: Username -> Email -> Maybe String
processUserInput rawName rawEmail
    | not (validateUsername rawName) = Nothing
    | not (validateEmail rawEmail) = Nothing
    | otherwise = Just $ formatUserDisplay (normalizedName, rawEmail)
    where normalizedName = normalizeUsername rawName

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr (\c acc -> 
    if c == delimiter 
    then []:acc 
    else (c:head acc):tail acc) [[]]