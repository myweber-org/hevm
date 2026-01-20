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
    in if total > 0 then unique / total else 0.0module WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

displayFrequencies :: [(String, Int)] -> String
displayFrequencies = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequencies . topNWords nmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map toLower <$> wordsList
        grouped = foldr countWord [] cleanedWords
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

formatResults :: [WordCount] -> String
formatResults counts = unlines $
    "Top 10 most frequent words:" : 
    map (\(word, count) -> word ++ ": " ++ show count) counts

processText :: String -> String
processText = formatResults . countWords
module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))
import Control.Arrow ((&&&))

type WordCount = (String, Int)

histogramBar :: Int -> Int -> String
histogramBar count maxCount = 
    let width = 50
        scaled = if maxCount > 0 then count * width `div` maxCount else 0
    in replicate scaled '█' ++ replicate (width - scaled) ' '

analyzeText :: String -> [WordCount]
analyzeText text = 
    let words' = filter (not . null) $ map normalize $ words text
        normalized = map toLower . filter isAlpha
        groups = group . sort $ words'
        counts = map (head &&& length) groups
    in take 10 $ sortOn (Down . snd) counts
  where
    normalize = map toLower . filter (\c -> isAlpha c || c == '\'')

displayHistogram :: [WordCount] -> IO ()
displayHistogram counts = 
    let maxCount = maximum (map snd counts)
        padTo n str = str ++ replicate (n - length str) ' '
        maxWordLength = maximum (map (length . fst) counts)
    in mapM_ (\(word, count) -> 
        putStrLn $ padTo maxWordLength word ++ " | " ++ 
                  histogramBar count maxCount ++ " " ++ show count) counts

processText :: String -> IO ()
processText text = do
    putStrLn "\nTop 10 most frequent words:"
    putStrLn $ replicate 60 '-'
    let results = analyzeText text
    displayHistogram results
    putStrLn $ replicate 60 '-'
    putStrLn $ "Total unique words: " ++ show (length results)