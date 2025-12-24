module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = filter (all isAlpha) wordsList
        frequencyMap = foldr countWord [] cleanedWords
    in take 10 $ sortOn (Down . snd) frequencyMap
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    
    countWord :: String -> [WordCount] -> [WordCount]
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

analyzeText :: String -> IO ()
analyzeText text = do
    putStrLn "Top 10 most frequent words:"
    mapM_ printWord $ countWords text
  where
    printWord (word, count) = 
        putStrLn $ word ++ ": " ++ show count

main :: IO ()
main = do
    let sampleText = "Hello world! This is a test. Hello again, world. Testing, testing, one two three."
    analyzeText sampleTextmodule WordFrequency where

import qualified Data.Map as Map
import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlphaNum

getTopWords :: Int -> String -> [(String, Int)]
getTopWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

processFile :: FilePath -> Int -> IO ()
processFile path n = do
    content <- readFile path
    let topWords = getTopWords n content
    mapM_ (\(word, freq) -> putStrLn $ word ++ ": " ++ show freq) topWords