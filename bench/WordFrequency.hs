module WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr insertWord Map.empty . words
  where
    insertWord w = Map.insertWith (+) (normalize w) 1
    normalize = map toLower . filter isAlphaNum

getTopWords :: Int -> WordCount -> [(String, Int)]
getTopWords n = take n . sortOn (Down . snd) . Map.toList

processTextFile :: FilePath -> IO ()
processTextFile filepath = do
    content <- readFile filepath
    let wordCounts = countWords content
        topWords = getTopWords 10 wordCounts
    
    putStrLn "Top 10 most frequent words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) topWords