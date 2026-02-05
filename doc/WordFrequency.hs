module WordFrequency where

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

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

analyzeText :: String -> IO ()
analyzeText text = do
  putStrLn "Top 10 most frequent words:"
  mapM_ printWord (topNWords 10 text)
  where
    printWord (word, count) = putStrLn $ word ++ ": " ++ show countmodule WordFrequency where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type FrequencyMap = Map.Map T.Text Int

countWords :: T.Text -> FrequencyMap
countWords text =
    let wordsList = T.words $ T.toLower $ T.filter (\c -> Char.isAlpha c || Char.isSpace c) text
    in Map.fromListWith (+) [(w, 1) | w <- wordsList]

getTopWords :: Int -> FrequencyMap -> [(T.Text, Int)]
getTopWords n freqMap =
    take n $ List.sortBy (\(_, cnt1) (_, cnt2) -> compare cnt2 cnt1) $ Map.toList freqMap

processFile :: FilePath -> IO ()
processFile filePath = do
    content <- TIO.readFile filePath
    let freqMap = countWords content
        topWords = getTopWords 10 freqMap
    
    putStrLn "Top 10 most frequent words:"
    mapM_ (\(word, count) -> TIO.putStrLn $ T.concat [word, T.pack ": ", T.pack (show count)]) topWords

main :: IO ()
main = do
    putStrLn "Enter file path:"
    filePath <- getLine
    processFile filePathmodule WordFrequency where

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
import Data.Map (Map)
import qualified Data.Map as Map

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord w = Map.insertWith (+) (normalize w) 1
    normalize = filter isAlpha . map toLower

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n $ sortOn (negate . snd) $ Map.toList (countWords text)

displayFrequency :: String -> IO ()
displayFrequency text = do
  putStrLn "Word Frequency Analysis:"
  mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) (topWords 10 text)
module TextUtils.WordFrequency where

import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (Down(..))

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord w m = Map.insertWith (+) (normalize w) 1 m
    normalize = map toLower . filter isAlpha

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

wordFrequencyReport :: String -> String
wordFrequencyReport text = unlines $
  "Total unique words: " ++ show (Map.size counts) :
  "Top 10 most frequent words:" :
  map formatEntry (topNWords 10 text)
  where
    counts = countWords text
    formatEntry (word, count) = "  " ++ word ++ ": " ++ show countmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map toLower wordsList
        grouped = groupCount cleaned
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha
    groupCount [] = []
    groupCount (w:ws) = 
        let (matches, rest) = span (== w) (w:ws)
        in (w, length matches) : groupCount rest

displayCounts :: [WordCount] -> String
displayCounts counts = unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

main :: IO ()
main = do
    putStrLn "Enter text to analyze word frequency:"
    input <- getContents
    let counts = countWords input
    putStrLn "\nTop 10 most frequent words:"
    putStrLn $ displayCounts countsmodule WordFrequency where

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
topNWords n text = take n $ sortOn (Down . snd) $ Map.toList $ countWords text

displayTopWords :: Int -> String -> IO ()
displayTopWords n text = do
  putStrLn $ "Top " ++ show n ++ " most frequent words:"
  mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) $ topNWords n text

sampleText :: String
sampleText = "Hello world! Hello Haskell. Haskell is fun. World says hello back."
module TextUtils.WordFrequency where

import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (Down(..))

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topNWords :: Int -> WordCount -> [(String, Int)]
topNWords n = take n . sortOn (Down . snd) . Map.toList

wordFrequencyReport :: Int -> String -> String
wordFrequencyReport n text =
  let counts = countWords text
      topWords = topNWords n counts
      totalWords = sum (Map.elems counts)
      uniqueWords = Map.size counts
  in unlines $
       [ "Text Analysis Report"
       , "==================="
       , "Total words: " ++ show totalWords
       , "Unique words: " ++ show uniqueWords
       , ""
       , "Top " ++ show n ++ " most frequent words:"
       ] ++
       map (\(word, count) -> word ++ ": " ++ show count) topWords

analyzeText :: String -> IO ()
analyzeText text = do
  putStrLn $ wordFrequencyReport 10 text
  let counts = countWords text
  putStrLn $ "Word density: " ++ show (fromIntegral (Map.size counts) / fromIntegral (sum (Map.elems counts)) :: Double)module WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)

wordFrequency :: String -> Map.Map String Int
wordFrequency text = 
    let wordsList = filter (not . null) $ map normalize $ words text
    in Map.fromListWith (+) [(word, 1) | word <- wordsList]
  where
    normalize = filter isAlpha . map toLower

mostFrequent :: String -> [(String, Int)]
mostFrequent text = 
    take 5 $ sortByFrequency $ Map.toList $ wordFrequency text
  where
    sortByFrequency = reverse . sortOn snd

analyzeText :: String -> IO ()
analyzeText input = do
    putStrLn "Top 5 most frequent words:"
    mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) $ mostFrequent inputmodule TextUtils.WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList
  where
    cleanWord = map toLower . filter isAlpha

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = 
    take n $ sortOn (Down . snd) $ Map.toList (countWords text)

wordFrequencyReport :: String -> String
wordFrequencyReport text =
    let freqList = topNWords 10 text
        totalWords = sum $ map snd freqList
    in unlines $
        "Word Frequency Analysis Report" :
        "==============================" :
        "" :
        map (\(word, count) -> word ++ ": " ++ show count ++ " (" ++ showPercentage count totalWords ++ "%)") freqList
  where
    showPercentage count total = 
        take 4 $ show ((fromIntegral count / fromIntegral total) * 100)

analyzeText :: String -> IO ()
analyzeText text = do
    putStrLn $ wordFrequencyReport text
    let freqMap = countWords text
    putStrLn $ "Total unique words: " ++ show (Map.size freqMap)
    putStrLn $ "Most frequent word: " ++ fst (head $ topNWords 1 text)
module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Environment (getArgs)

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map toLower wordsList
        grouped = foldr (\word acc -> insertWord word acc) [] cleaned
    in sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha
    insertWord word [] = [(word, 1)]
    insertWord word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : insertWord word rest

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

processFile :: FilePath -> IO ()
processFile filename = do
    content <- readFile filename
    let frequencies = countWords content
    putStrLn $ formatOutput frequencies

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> processFile filename
        _ -> putStrLn "Usage: wordfrequency <filename>"
module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))
import qualified Data.Map.Strict as Map

type Histogram = [(String, Int)]

analyzeText :: String -> Histogram
analyzeText text = 
    let wordsList = filter (not . null) $ map normalize $ words text
        freqMap = foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList
        sorted = sortOn (Down . snd) $ Map.toList freqMap
    in take 10 sorted
  where
    normalize = filter isAlpha . map toLower

displayHistogram :: Histogram -> String
displayHistogram hist = unlines $ map renderBar hist
  where
    maxFreq = maximum $ map snd hist
    renderBar (word, count) = 
        let barLength = round ((fromIntegral count / fromIntegral maxFreq) * 50)
            bar = replicate barLength '█'
        in word ++ " " ++ bar ++ " " ++ show count

main :: IO ()
main = do
    putStrLn "Enter text to analyze (press Ctrl+D when done):"
    content <- getContents
    let histogram = analyzeText content
    putStrLn "\nTop 10 most frequent words:"
    putStrLn $ displayHistogram histogram