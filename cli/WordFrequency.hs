module WordFrequency where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map clean $ words text
        cleaned = map toLower <$> wordsList
        freqMap = foldr (\w m -> insertWord w m) [] cleaned
    in take 10 $ sortOn (Down . snd) freqMap
  where
    clean = filter (\c -> isAlphaNum c || c == '\'')
    
    insertWord :: String -> [WordCount] -> [WordCount]
    insertWord w [] = [(w, 1)]
    insertWord w ((word, count):rest)
        | w == word = (word, count + 1) : rest
        | otherwise = (word, count) : insertWord w rest

displayCounts :: [WordCount] -> String
displayCounts counts = unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is fun. World says hello."
    putStrLn "Top 10 most frequent words:"
    putStrLn $ displayCounts $ countWords sampleText