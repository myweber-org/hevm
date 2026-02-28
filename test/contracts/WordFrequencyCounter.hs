module WordFrequencyCounter where

import Data.Char (toLower)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type Word = String

countWordFrequencies :: [Word] -> [(Word, Int)]
countWordFrequencies words =
  let normalized = map (map toLower) words
      sorted = sort normalized
      grouped = group sorted
      frequencies = map (\ws -> (head ws, length ws)) grouped
      sortedFrequencies = sortOn (Down . snd) frequencies
  in sortedFrequencies

filterByMinimumFrequency :: Int -> [(Word, Int)] -> [(Word, Int)]
filterByMinimumFrequency minFreq = filter (\(_, count) -> count >= minFreq)

main :: IO ()
main = do
  let sampleText = ["the", "quick", "brown", "fox", "jumps", "over", 
                    "the", "lazy", "dog", "the", "fox", "fox"]
      frequencies = countWordFrequencies sampleText
      filtered = filterByMinimumFrequency 2 frequencies
  
  putStrLn "All word frequencies:"
  mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) frequencies
  
  putStrLn "\nWords appearing at least 2 times:"
  mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) filteredmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

-- | Count frequency of each word in a string
countWords :: String -> [WordCount]
countWords text = 
    let words' = filter (not . null) $ map cleanWord $ words text
        grouped = foldr countHelper [] words'
    in sortOn (Down . snd) grouped
  where
    cleanWord = map toLower . filter isAlpha
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

-- | Get top N most frequent words
topNWords :: Int -> String -> [WordCount]
topNWords n text = take n $ countWords text

-- | Pretty print word frequencies
printFrequencies :: [WordCount] -> IO ()
printFrequencies counts = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

-- | Example usage
exampleText :: String
exampleText = "Hello world! Hello Haskell. Haskell is functional. World is imperative."module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type WordCount = Map.Map T.Text Int

countWords :: T.Text -> WordCount
countWords = Map.fromListWith (+) . map (\w -> (w, 1)) . filter (not . T.null) . map normalize . T.words
  where
    normalize = T.toLower . T.filter Char.isLetter

sortByFrequency :: WordCount -> [(T.Text, Int)]
sortByFrequency = List.sortBy (\(_, c1) (_, c2) -> compare c2 c1) . Map.toList

formatOutput :: [(T.Text, Int)] -> T.Text
formatOutput = T.unlines . map (\(word, count) -> T.pack (show count) <> " " <> word)

processText :: T.Text -> T.Text
processText = formatOutput . sortByFrequency . countWords

main :: IO ()
main = do
    input <- TIO.getContents
    TIO.putStr $ processText input