module WordFrequencyCounter where

import qualified Data.Map as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordFrequency = Map.Map String Int

countWords :: String -> WordFrequency
countWords text = 
    let words' = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty words'

getTopWords :: Int -> WordFrequency -> [(String, Int)]
getTopWords n freqMap = 
    take n $ sortOn (Down . snd) $ Map.toList freqMap

filterByMinFrequency :: Int -> WordFrequency -> WordFrequency
filterByMinFrequency minFreq = Map.filter (>= minFreq)

analyzeText :: String -> Int -> Int -> IO ()
analyzeText text topN minFreq = do
    let freqMap = countWords text
    let filteredMap = filterByMinFrequency minFreq freqMap
    let topWords = getTopWords topN filteredMap
    
    putStrLn "Top words by frequency:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) topWords
    
    putStrLn $ "\nTotal unique words: " ++ show (Map.size freqMap)
    putStrLn $ "Words meeting minimum frequency (" ++ show minFreq ++ "): " ++ show (Map.size filteredMap)

sampleText :: String
sampleText = "The quick brown fox jumps over the lazy dog. The dog barks at the fox."

main :: IO ()
main = do
    putStrLn "Word Frequency Analysis"
    putStrLn "======================="
    analyzeText sampleText 5 2