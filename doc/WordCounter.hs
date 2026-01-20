
module WordCounter where

import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import Data.List (sortOn)

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter (not . isSpace)
    toLower c
        | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
        | otherwise = c

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (negate . snd) $ Map.toList (countWords text)

wordFrequencyReport :: String -> String
wordFrequencyReport text = unlines $
    "Total unique words: " ++ show (Map.size counts) :
    "Top 10 words:" :
    map formatWord (topNWords 10 text)
  where
    counts = countWords text
    formatWord (word, count) = "  " ++ word ++ ": " ++ show count

-- Example usage
sampleText :: String
sampleText = "Hello world! This is a test. Hello again, world!"

main :: IO ()
main = do
    putStrLn "Word Frequency Analysis"
    putStrLn "======================="
    putStrLn $ wordFrequencyReport sampleText
    putStrLn "\nRaw word counts:"
    print $ countWords sampleText