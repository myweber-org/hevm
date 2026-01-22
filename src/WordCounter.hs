
module WordCounter where

import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

countWords :: String -> [WordFreq]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        freqMap = foldr (\w m -> insertWord w m) [] wordsList
    in take 10 $ sortOn (Down . snd) freqMap
  where
    cleanWord = map toLower . filter isAlpha
    
    insertWord :: String -> [WordFreq] -> [WordFreq]
    insertWord w [] = [(w, 1)]
    insertWord w ((word, count):rest)
        | w == word = (word, count + 1) : rest
        | otherwise = (word, count) : insertWord w rest

printTopWords :: String -> IO ()
printTopWords text = do
    putStrLn "Top 10 most frequent words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) 
          (countWords text)

sampleText :: String
sampleText = "This is a sample text. This text contains words. Some words repeat. This is intentional."

main :: IO ()
main = printTopWords sampleText
module WordCounter where

import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter (not . isPunctuation)
    toLower c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c
    isPunctuation c = c `elem` ".,!?;:\"'()[]{}"

topNWords :: Int -> WordCount -> [(String, Int)]
topNWords n = take n . sortOn (Down . snd) . Map.toList

wordFrequencyReport :: String -> Int -> String
wordFrequencyReport text n =
  let counts = countWords text
      topWords = topNWords n counts
      totalWords = Map.foldl' (+) 0 counts
      uniqueWords = Map.size counts
  in unlines $
     [ "Text Analysis Report"
     , "===================="
     , "Total words: " ++ show totalWords
     , "Unique words: " ++ show uniqueWords
     , ""
     , "Top " ++ show n ++ " most frequent words:"
     ] ++
     map (\(word, count) -> word ++ ": " ++ show count) topWords

analyzeFile :: FilePath -> Int -> IO ()
analyzeFile path n = do
  content <- readFile path
  putStrLn $ wordFrequencyReport content n

main :: IO ()
main = do
  putStrLn "Enter text to analyze (end with Ctrl+D on empty line):"
  content <- getContents
  putStrLn $ wordFrequencyReport content 10