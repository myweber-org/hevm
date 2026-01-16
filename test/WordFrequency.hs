module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = filter (all isAlpha) wordsList
        wordMap = foldl countWord emptyMap cleanedWords
    in take 10 $ sortOn (Down . snd) $ toList wordMap
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    
    emptyMap = []
    
    countWord :: [WordCount] -> String -> [WordCount]
    countWord [] word = [(word, 1)]
    countWord ((w, c):rest) word
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord rest word
    
    toList = id

printWordFrequencies :: String -> IO ()
printWordFrequencies text = do
    let frequencies = countWords text
    putStrLn "Top 10 most frequent words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) frequencies

processTextFile :: FilePath -> IO ()
processTextFile filePath = do
    content <- readFile filePath
    printWordFrequencies content