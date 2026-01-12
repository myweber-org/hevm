module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map toLower <$> wordsList
        grouped = foldr (\w m -> insertWord w m) [] cleaned
    in sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha
    insertWord w [] = [(w, 1)]
    insertWord w ((x, n):xs)
        | w == x = (x, n+1) : xs
        | otherwise = (x, n) : insertWord w xs

formatResults :: [WordCount] -> String
formatResults counts = unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

main :: IO ()
main = do
    putStrLn "Enter text to analyze (press Ctrl+D when finished):"
    input <- getContents
    let frequencies = countWords input
    putStrLn "\nWord frequencies (sorted by count):"
    putStrLn $ formatResults frequenciesmodule TextUtils.WordFrequency where

import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (Down(..))

type WordCount = Map String Int

-- | Count frequency of words in a text string
countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

-- | Get top N most frequent words
topNWords :: Int -> WordCount -> [(String, Int)]
topNWords n = take n . sortOn (Down . snd) . Map.toList

-- | Generate frequency report
wordFrequencyReport :: Int -> String -> String
wordFrequencyReport n text =
    let counts = countWords text
        topWords = topNWords n counts
        totalWords = Map.size counts
        uniqueWords = sum (Map.elems counts)
    in unlines $
        [ "Word Frequency Analysis"
        , "======================"
        , "Total unique words: " ++ show totalWords
        , "Total words: " ++ show uniqueWords
        , ""
        , "Top " ++ show n ++ " most frequent words:"
        , "----------------------------------------"
        ] ++
        map (\(word, count) -> word ++ ": " ++ show count) topWords

-- | Calculate word diversity (unique words / total words)
wordDiversity :: WordCount -> Double
wordDiversity counts =
    let unique = fromIntegral $ Map.size counts
        total = fromIntegral $ sum (Map.elems counts)
    in if total > 0 then unique / total else 0.0