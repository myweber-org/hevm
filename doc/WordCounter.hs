
module WordCounter (countWords, mostFrequent) where

import Data.Char (isAlpha, toLower)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

cleanWord :: String -> String
cleanWord = map toLower . filter isAlpha

countWords :: String -> [WordCount]
countWords text = 
    let words' = words text
        cleaned = map cleanWord words'
        nonEmpty = filter (not . null) cleaned
        grouped = group . sort $ nonEmpty
    in map (\ws -> (head ws, length ws)) grouped

mostFrequent :: Int -> String -> [WordCount]
mostFrequent n text = 
    take n . sortOn (Down . snd) $ countWords textmodule WordCounter where

import Data.Char (isSpace)
import Data.List (group, sort)

countWords :: String -> [(String, Int)]
countWords text = map (\xs -> (head xs, length xs)) 
                 . group 
                 . sort 
                 . words 
                 $ map toLowerClean text
  where
    toLowerClean c
      | isSpace c = ' '
      | otherwise = toLower c

toLower :: Char -> Char
toLower c
  | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
  | otherwise = c

displayCounts :: [(String, Int)] -> String
displayCounts counts = unlines 
                     $ map (\(word, count) -> word ++ ": " ++ show count) 
                     $ sortByFrequency counts

sortByFrequency :: [(String, Int)] -> [(String, Int)]
sortByFrequency = sortBy (\(_, c1) (_, c2) -> compare c2 c1)