module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map toLower <$> wordsList
        counts = foldl incrementCount [] cleaned
    in sortOn (Down . snd) counts
  where
    cleanWord = filter isAlpha
    incrementCount [] word = [(word, 1)]
    incrementCount ((w, c):rest) word
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : incrementCount rest word

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map formatLine counts
  where
    formatLine (word, count) = word ++ ": " ++ show count

main :: IO ()
main = do
    putStrLn "Enter text to analyze word frequency:"
    input <- getContents
    let frequencies = countWords input
    putStrLn "\nWord frequencies (sorted by count):"
    putStrLn $ formatOutput frequencies