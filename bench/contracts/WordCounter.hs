
module WordCounter where

import Data.Char (isSpace)
import Data.List (group, sort)

countWords :: String -> [(String, Int)]
countWords = map (\ws -> (head ws, length ws)) 
           . group 
           . sort 
           . words 
           . map normalize
  where
    normalize c
      | c `elem` ".,!?;:\"" = ' '
      | otherwise = toLower c

wordFrequency :: String -> IO ()
wordFrequency text = do
  let counts = countWords text
  putStrLn "Word frequencies:"
  mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) counts
  putStrLn $ "Total unique words: " ++ show (length counts)module WordCounter where

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
  "Top 10 most frequent words:" :
  map formatWord (topNWords 10 text)
  where
    counts = countWords text
    formatWord (word, count) = "  " ++ word ++ ": " ++ show count

processTextFile :: FilePath -> IO ()
processTextFile path = do
  content <- readFile path
  putStrLn $ wordFrequencyReport content