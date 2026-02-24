module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

-- | Count frequency of each word in a string
countWordFrequencies :: String -> [WordFreq]
countWordFrequencies text =
    let wordsList = filter (not . null) $ map normalize $ splitWords text
        freqMap = foldr (\word acc -> insertWord word acc) [] wordsList
    in sortOn (Down . snd) freqMap
  where
    splitWords = words . map (\c -> if isAlpha c then toLower c else ' ')
    normalize = filter isAlpha
    
    insertWord :: String -> [WordFreq] -> [WordFreq]
    insertWord word [] = [(word, 1)]
    insertWord word ((w, count):rest)
        | w == word = (w, count + 1) : rest
        | otherwise = (w, count) : insertWord word rest

-- | Get top N most frequent words
topNWords :: Int -> String -> [WordFreq]
topNWords n text = take n $ countWordFrequencies text

-- | Calculate word frequency statistics
wordStats :: String -> (Int, Int, Double)
wordStats text =
    let freqs = countWordFrequencies text
        totalWords = sum $ map snd freqs
        uniqueWords = length freqs
        avgFrequency = if uniqueWords > 0 
                      then fromIntegral totalWords / fromIntegral uniqueWords 
                      else 0.0
    in (totalWords, uniqueWords, avgFrequency)module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = Map.fromListWith (+) . map (\w -> (w, 1)) . words . normalize
  where
    normalize = map Char.toLower . filter (\c -> Char.isAlpha c || Char.isSpace c)

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n $ List.sortBy (\(_, a) (_, b) -> compare b a) $ Map.toList $ countWords text

displayTopWords :: Int -> String -> IO ()
displayTopWords n text = do
    putStrLn $ "Top " ++ show n ++ " words:"
    mapM_ (\(w, c) -> putStrLn $ "  " ++ w ++ ": " ++ show c) $ topWords n text

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is fun. World says hello back."
    displayTopWords 5 sampleText