module WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)

countWords :: String -> Map.Map String Int
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanWord = map toLower . filter isAlpha
    in Map.fromListWith (+) [(word, 1) | word <- wordsList]

mostFrequent :: Map.Map String Int -> [(String, Int)]
mostFrequent wordMap = 
    take 5 $ reverse $ sortBy (comparing snd) $ Map.toList wordMap
  where
    comparing f x y = compare (f x) (f y)

displayResults :: String -> IO ()
displayResults text = do
    let freqMap = countWords text
        topWords = mostFrequent freqMap
    
    putStrLn "Top 5 most frequent words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) topWords
    
    putStrLn $ "\nTotal unique words: " ++ show (Map.size freqMap)