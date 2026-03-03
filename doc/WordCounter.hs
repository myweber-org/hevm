
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
    take n . sortOn (Down . snd) $ countWords text