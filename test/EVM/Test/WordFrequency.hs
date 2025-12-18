module WordFrequency where

import Data.Char (toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = words $ map normalizeChar text
        normalized = map normalizeWord wordsList
        grouped = groupCounts normalized
    in take 10 $ sortOn (Down . snd) grouped
  where
    normalizeChar c
        | c `elem` ",.!?;:\"()[]{}" = ' '
        | otherwise = toLower c
    
    normalizeWord w = filter (`notElem` "'-") w
    
    groupCounts :: [String] -> [WordCount]
    groupCounts = foldr countHelper []
    
    countHelper :: String -> [WordCount] -> [WordCount]
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

analyzeText :: String -> IO ()
analyzeText input = do
    putStrLn "Top 10 most frequent words:"
    mapM_ printWord (countWords input)
  where
    printWord (word, count) = 
        putStrLn $ word ++ ": " ++ show count