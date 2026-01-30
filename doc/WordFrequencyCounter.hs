module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map normalize $ words text
        normalized = map toLower . filter isAlpha
        normalize = normalized
        frequencies = foldr countWord [] wordsList
        countWord word [] = [(word, 1)]
        countWord word ((w, c):rest)
            | word == w = (w, c + 1) : rest
            | otherwise = (w, c) : countWord word rest
    in sortOn (Down . snd) frequencies

topNWords :: Int -> String -> [WordCount]
topNWords n = take n . countWords

testText :: String
testText = "The quick brown fox jumps over the lazy dog. The dog barks at the fox."

main :: IO ()
main = do
    putStrLn "Word frequencies:"
    mapM_ print $ countWords testText
    putStrLn "\nTop 3 words:"
    mapM_ print $ topNWords 3 testText