module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = filter (all isAlpha) wordsList
        grouped = groupWords cleaned
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    
    groupWords :: [String] -> [WordCount]
    groupWords = foldr incrementCount []
    
    incrementCount :: String -> [WordCount] -> [WordCount]
    incrementCount word [] = [(word, 1)]
    incrementCount word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : incrementCount word rest

displayResults :: [WordCount] -> IO ()
displayResults counts = do
    putStrLn "Top 10 most frequent words:"
    putStrLn "---------------------------"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

analyzeText :: String -> IO ()
analyzeText text = do
    let frequencies = countWords text
    displayResults frequencies