
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
    processText input