module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

-- | Count frequency of each word in a text
countWords :: String -> [WordCount]
countWords text = 
    let words' = filter (not . null) $ map cleanWord $ words text
        cleaned = filter (all isAlpha) words'
    in map (\ws -> (head ws, length ws)) 
       $ group 
       $ sort 
       $ map (map toLower) cleaned
  where
    cleanWord = takeWhile isAlpha . dropWhile (not . isAlpha)

-- | Display word frequencies as a simple histogram
displayHistogram :: [WordCount] -> IO ()
displayHistogram counts = 
    mapM_ (\(word, count) -> 
        putStrLn $ word ++ ": " ++ replicate count '*') 
    $ take 20 
    $ sortOn (Down . snd) counts

-- | Main analysis function
analyzeText :: String -> IO ()
analyzeText text = do
    putStrLn "Top 20 most frequent words:"
    displayHistogram $ countWords text
    let total = sum $ map snd $ countWords text
    putStrLn $ "\nTotal words analyzed: " ++ show total

-- | Example usage
exampleAnalysis :: IO ()
exampleAnalysis = do
    let sampleText = "Hello world! This is a test. Hello again world. Testing testing one two three."
    analyzeText sampleTextmodule WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

displayFrequencies :: [(String, Int)] -> String
displayFrequencies = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequencies . topWords nmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        grouped = foldr countWord [] cleanedWords
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

displayFrequency :: [WordCount] -> String
displayFrequency counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

analyzeText :: String -> String
analyzeText = displayFrequency . countWords