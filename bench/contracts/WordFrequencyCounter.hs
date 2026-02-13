
module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type WordCount = Map.Map T.Text Int

countWords :: T.Text -> WordCount
countWords text =
    let wordsList = T.words $ T.toLower $ T.filter (\c -> Char.isLetter c || Char.isSpace c) text
    in Map.fromListWith (+) [(w, 1) | w <- wordsList]

sortByFrequency :: WordCount -> [(T.Text, Int)]
sortByFrequency = List.sortBy (\(_, a) (_, b) -> compare b a) . Map.toList

filterByMinFrequency :: Int -> WordCount -> WordCount
filterByMinFrequency minFreq = Map.filter (>= minFreq)

getTopNWords :: Int -> WordCount -> [(T.Text, Int)]
getTopNWords n = take n . sortByFrequency

printWordFrequencies :: [(T.Text, Int)] -> IO ()
printWordFrequencies frequencies = do
    putStrLn "Word frequencies:"
    mapM_ (\(word, count) -> TIO.putStrLn $ T.concat [word, T.pack ": ", T.pack (show count)]) frequencies

processText :: T.Text -> IO ()
processText text = do
    let wordCount = countWords text
    let sorted = sortByFrequency wordCount
    let filtered = filterByMinFrequency 2 wordCount
    let top10 = getTopNWords 10 wordCount
    
    putStrLn "\nAll words sorted by frequency:"
    printWordFrequencies sorted
    
    putStrLn "\nWords appearing at least twice:"
    printWordFrequencies $ sortByFrequency filtered
    
    putStrLn "\nTop 10 most frequent words:"
    printWordFrequencies top10

main :: IO ()
main = do
    putStrLn "Enter text (press Ctrl+D on empty line to finish):"
    input <- TIO.getContents
    processText inputmodule WordFrequencyCounter where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlphaNum)

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlphaNum

mostFrequent :: WordCount -> [(String, Int)]
mostFrequent = take 5 . sortByFrequency . Map.toList
  where
    sortByFrequency = reverse . sortOn snd

displayResults :: [(String, Int)] -> IO ()
displayResults freqList = do
    putStrLn "Top 5 most frequent words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) freqList

processText :: String -> IO ()
processText text = do
    let counts = countWords text
    let topWords = mostFrequent counts
    displayResults topWords
    putStrLn $ "Total unique words: " ++ show (Map.size counts)

main :: IO ()
main = do
    putStrLn "Enter text to analyze (press Ctrl+D when done):"
    input <- getContents
    processText inputmodule WordFrequencyCounter where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = [(String, Int)]

countWords :: String -> WordCount
countWords text =
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        frequencyMap = foldl updateCount [] cleanedWords
    in sortOn (Down . snd) frequencyMap
  where
    cleanWord = filter (\c -> isAlphaNum c || c == '\'')
    updateCount [] word = [(word, 1)]
    updateCount ((w, c):rest) word
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : updateCount rest word

displayCounts :: WordCount -> IO ()
displayCounts counts = do
    putStrLn "Word frequencies:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. This is a test of the word frequency counter. Hello again!"
    let frequencies = countWords sampleText
    displayCounts frequencies