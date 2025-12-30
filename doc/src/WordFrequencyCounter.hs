module WordFrequencyCounter where

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

sortByFrequency :: WordCount -> [(String, Int)]
sortByFrequency = sortOn (negate . snd) . Map.toList

formatOutput :: [(String, Int)] -> String
formatOutput = unlines . map formatPair
  where
    formatPair (word, count) = word ++ ": " ++ show count

analyzeText :: String -> String
analyzeText = formatOutput . sortByFrequency . countWords

main :: IO ()
main = do
    input <- getContents
    putStrLn $ analyzeText inputmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

countWords :: String -> [WordFreq]
countWords text = 
    let wordsList = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        grouped = group $ sort wordsList
    in sortOn (Down . snd) $ map (\ws -> (head ws, length ws)) grouped

printHistogram :: [WordFreq] -> IO ()
printHistogram freqs = do
    putStrLn "Word Frequency Histogram:"
    putStrLn "=========================="
    mapM_ (\(word, count) -> 
        putStrLn $ word ++ ": " ++ replicate count '*' ++ " (" ++ show count ++ ")") 
        freqs
    putStrLn "=========================="

processText :: String -> IO ()
processText text = do
    let frequencies = countWords text
    putStrLn $ "Total unique words: " ++ show (length frequencies)
    putStrLn $ "Total words: " ++ show (sum $ map snd frequencies)
    printHistogram $ take 10 frequencies

main :: IO ()
main = do
    let sampleText = "Hello world hello haskell world programming haskell functional programming"
    processText sampleText