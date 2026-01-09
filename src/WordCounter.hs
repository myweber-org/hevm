
module WordCounter where

import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

countWords :: String -> [WordFreq]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        freqMap = foldr (\w m -> insertWord w m) [] wordsList
    in take 10 $ sortOn (Down . snd) freqMap
  where
    cleanWord = map toLower . filter isAlpha
    
    insertWord :: String -> [WordFreq] -> [WordFreq]
    insertWord w [] = [(w, 1)]
    insertWord w ((word, count):rest)
        | w == word = (word, count + 1) : rest
        | otherwise = (word, count) : insertWord w rest

printTopWords :: String -> IO ()
printTopWords text = do
    putStrLn "Top 10 most frequent words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) 
          (countWords text)

sampleText :: String
sampleText = "This is a sample text. This text contains words. Some words repeat. This is intentional."

main :: IO ()
main = printTopWords sampleText