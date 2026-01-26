module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        wordMap = foldr (\word acc -> 
            case lookup word acc of
                Just count -> (word, count + 1) : filter ((/= word) . fst) acc
                Nothing -> (word, 1) : acc
            ) [] wordsList
    in sortOn (Down . snd) wordMap
  where
    cleanWord = map toLower . filter isAlpha

formatResults :: [WordCount] -> String
formatResults counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

analyzeText :: String -> String
analyzeText = formatResults . countWords

main :: IO ()
main = do
    putStrLn "Enter text to analyze (press Ctrl+D when done):"
    input <- getContents
    putStrLn "\nWord frequencies:"
    putStrLn $ analyzeText input