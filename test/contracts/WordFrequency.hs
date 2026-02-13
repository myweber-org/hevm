
module TextUtils.WordFrequency where

import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (Down(..))

type WordCount = Map String Int

-- | Count word frequencies in a text string
--   Words are normalized to lowercase and non-alphabetic characters are ignored
countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word acc =
        let normalized = normalizeWord word
        in if null normalized
            then acc
            else Map.insertWith (+) normalized 1 acc
    
    normalizeWord = map toLower . filter isAlpha

-- | Get top N most frequent words
topWords :: Int -> WordCount -> [(String, Int)]
topWords n = take n . sortOn (Down . snd) . Map.toList

-- | Pretty print word frequencies
printFrequencies :: [(String, Int)] -> String
printFrequencies = unlines . map (\(w, c) -> w ++ ": " ++ show c)

-- | Complete analysis pipeline
analyzeText :: Int -> String -> String
analyzeText n text = 
    let frequencies = countWords text
        top = topWords n frequencies
        total = Map.size frequencies
    in "Total unique words: " ++ show total ++ "\n\n" ++
       "Top " ++ show n ++ " words:\n" ++ 
       printFrequencies top

-- | Example usage
exampleAnalysis :: IO ()
exampleAnalysis = do
    let text = "Hello world! Hello Haskell. World of functional programming."
    putStrLn $ analyzeText 5 textmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = filter (all isAlpha) wordsList
        grouped = foldr countHelper [] cleaned
    in sortOn (Down . snd) grouped
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | w == word = (w, c+1):rest
        | otherwise = (w, c):countHelper word rest

formatResults :: [WordCount] -> String
formatResults counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

processText :: String -> String
processText = formatResults . countWords

main :: IO ()
main = do
    input <- getContents
    putStr $ processText input