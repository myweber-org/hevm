module WordCounter where

import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, toLower)
import Data.List (words)

countWords :: String -> Map.Map String Int
countWords text = 
    let cleanedWords = map (map toLower . filter isAlpha) (words text)
        validWords = filter (not . null) cleanedWords
    in Map.fromListWith (+) [(word, 1) | word <- validWords]

displayWordCount :: Map.Map String Int -> String
displayWordCount wordMap = 
    unlines [word ++ ": " ++ show count | (word, count) <- Map.toList wordMap]

processText :: String -> String
processText text = displayWordCount (countWords text)module WordCounter where

import Data.Char (isSpace)
import Data.List (group, sort)

countWords :: String -> [(String, Int)]
countWords = map (\xs -> (head xs, length xs)) . group . sort . words . normalize
  where
    normalize = unwords . words . map toLowerSpace
    toLowerSpace c
      | isSpace c = ' '
      | otherwise = toLower c

toLower :: Char -> Char
toLower c
  | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
  | otherwise = c

printWordCounts :: String -> IO ()
printWordCounts text = do
  let counts = countWords text
  putStrLn "Word frequencies:"
  mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts