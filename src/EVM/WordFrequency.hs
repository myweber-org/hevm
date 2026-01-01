module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n $ sortOn (negate . snd) $ Map.toList (countWords text)

displayFrequency :: String -> IO ()
displayFrequency text = do
  putStrLn "Top 10 most frequent words:"
  mapM_ printWord (topWords 10 text)
  where
    printWord (word, count) = putStrLn $ word ++ ": " ++ show countmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = filter (all isAlpha) wordsList
    in sortOn (Down . snd) $ countOccurrences cleanedWords
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')

countOccurrences :: [String] -> [WordCount]
countOccurrences = foldr incrementCount []
  where
    incrementCount word counts = 
        case lookup word counts of
            Just n -> (word, n + 1) : filter ((/= word) . fst) counts
            Nothing -> (word, 1) : counts

getTopWords :: Int -> String -> [WordCount]
getTopWords n text = take n $ countWords text

displayResults :: [WordCount] -> String
displayResults counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

analyzeText :: String -> String
analyzeText text = 
    let totalWords = length $ words text
        uniqueWords = length $ countWords text
        topWords = getTopWords 5 text
    in unlines $
        [ "Text Analysis Results:"
        , "Total words: " ++ show totalWords
        , "Unique words: " ++ show uniqueWords
        , ""
        , "Top 5 most frequent words:"
        ] ++ map (\(w,c) -> "  " ++ w ++ ": " ++ show c) topWordsmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map clean $ words text
        cleaned = filter (all isAlpha) wordsList
        grouped = group $ sort cleaned
    in sortOn (Down . snd) $ map (\ws -> (head ws, length ws)) grouped
  where
    clean = map toLower . filter (\c -> isAlpha c || c == '\'')

histogram :: [WordCount] -> String
histogram counts = unlines $ map showBar counts
  where
    maxCount = maximum $ map snd counts
    scale = 50.0 / fromIntegral maxCount
    showBar (word, count) = 
        word ++ " " ++ replicate (round (fromIntegral count * scale)) '█' ++ " " ++ show count

printWordFrequency :: String -> IO ()
printWordFrequency text = do
    let counts = countWords text
    putStrLn "Word Frequency Analysis:"
    putStrLn "========================"
    putStrLn $ histogram $ take 20 counts
    putStrLn $ "\nTotal unique words: " ++ show (length counts)module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map toLower <$> wordsList
        frequencyMap = foldr (\word acc -> 
            case lookup word acc of
                Just count -> (word, count + 1) : filter ((/= word) . fst) acc
                Nothing -> (word, 1) : acc
            ) [] cleanedWords
    in sortOn (Down . snd) frequencyMap
  where
    cleanWord = filter isAlpha

displayFrequency :: [WordCount] -> String
displayFrequency counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

analyzeText :: String -> String
analyzeText text = 
    let counts = countWords text
    in "Word Frequency Analysis:\n" ++ displayFrequency countsmodule WordFrequency where

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

analyzeText :: String -> IO ()
analyzeText text = do
  putStrLn "Top 10 most frequent words:"
  mapM_ printWord (topNWords 10 text)
  where
    printWord (word, count) = putStrLn $ word ++ ": " ++ show count