module TextUtils.WordFrequency where

import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

-- | Count frequency of words in a text string
--   Words are normalized to lowercase and non-alphabetic characters are ignored
countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map normalize $ words text
        normalized = map toLower <$> wordsList
        grouped = foldr countWord [] normalized
    in sortOn (Down . snd) grouped
  where
    normalize = filter isAlpha
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

-- | Get top N most frequent words
topWords :: Int -> String -> [WordCount]
topWords n text = take n $ countWords text

-- | Pretty print word frequencies
printFrequencies :: [WordCount] -> IO ()
printFrequencies counts = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts