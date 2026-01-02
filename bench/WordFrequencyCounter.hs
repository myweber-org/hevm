module WordFrequencyCounter where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char (isAlpha, toLower)

countWords :: T.Text -> Map.Map T.Text Int
countWords text = Map.fromListWith (+) [(normalize word, 1) | word <- T.words text]
  where
    normalize = T.filter isAlpha . T.toLower

processFile :: FilePath -> IO (Map.Map T.Text Int)
processFile path = do
    content <- TIO.readFile path
    return $ countWords content

displayTopN :: Int -> Map.Map T.Text Int -> IO ()
displayTopN n freqMap = do
    let sorted = take n $ Map.toDescList freqMap
    mapM_ (\(word, count) -> putStrLn $ T.unpack word ++ ": " ++ show count) sorted

main :: IO ()
main = do
    putStrLn "Enter file path:"
    path <- getLine
    freqMap <- processFile path
    putStrLn "Enter number of top words to display:"
    n <- readLn
    displayTopN n freqMapmodule WordFrequencyCounter where

import qualified Data.Map as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordFrequency = Map.Map String Int

countWords :: String -> WordFrequency
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanWord = filter isAlpha . map toLower
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList

getTopWords :: Int -> WordFrequency -> [(String, Int)]
getTopWords n freqMap = 
    take n $ sortOn (Down . snd) $ Map.toList freqMap

filterByMinFrequency :: Int -> WordFrequency -> WordFrequency
filterByMinFrequency minFreq = Map.filter (>= minFreq)

wordFrequencyReport :: String -> Int -> Int -> IO ()
wordFrequencyReport text topN minFreq = do
    let frequencies = countWords text
        filtered = filterByMinFrequency minFreq frequencies
        topWords = getTopWords topN filtered
    
    putStrLn "Word Frequency Report:"
    putStrLn "======================"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) topWords
    putStrLn $ "Total unique words: " ++ show (Map.size filtered)

-- Example usage
sampleText :: String
sampleText = "Hello world! Hello Haskell. Haskell is functional. World is imperative."

main :: IO ()
main = wordFrequencyReport sampleText 5 1