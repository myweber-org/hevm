module WordFrequencyCounter where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = [(String, Int)]

countWords :: String -> WordCount
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        grouped = foldr countHelper [] cleanedWords
    in sortOn (Down . snd) grouped
  where
    cleanWord = filter (\c -> isAlphaNum c || c == '\'')
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

displayTopWords :: Int -> WordCount -> IO ()
displayTopWords n counts = 
    let topN = take n counts
    in mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) topN

analyzeText :: String -> IO ()
analyzeText text = do
    putStrLn "Word frequency analysis:"
    let frequencies = countWords text
    displayTopWords 10 frequencies
    putStrLn $ "\nTotal unique words: " ++ show (length frequencies)

sampleText :: String
sampleText = 
    "This is a sample text for word frequency analysis. " ++
    "This text contains repeated words like 'sample' and 'text'. " ++
    "Word frequency analysis helps understand common terms in documents."

main :: IO ()
main = do
    putStrLn "Analyzing sample text..."
    analyzeText sampleText