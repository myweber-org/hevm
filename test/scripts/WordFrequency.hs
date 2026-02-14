module WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, toLower)

countWords :: String -> Map.Map String Int
countWords text = 
    let wordsList = filter (not . null) $ map (map toLower . filter isAlpha) $ words text
    in Map.fromListWith (+) [(word, 1) | word <- wordsList]

mostFrequent :: Map.Map String Int -> [(String, Int)]
mostFrequent wordMap = 
    take 5 $ reverse $ sortOn snd $ Map.toList wordMap

displayResults :: String -> IO ()
displayResults text = do
    let freqMap = countWords text
        topWords = mostFrequent freqMap
    
    putStrLn "Top 5 most frequent words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) topWords
    
    putStrLn $ "\nTotal unique words: " ++ show (Map.size freqMap)