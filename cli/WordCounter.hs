module WordCounter where

import Data.Char (isSpace)
import Data.List (group, sort)

countWords :: String -> [(String, Int)]
countWords text = 
    let wordsList = filter (not . all isSpace) $ splitWords text
        wordCounts = map (\ws -> (head ws, length ws)) . group . sort $ wordsList
    in wordCounts

splitWords :: String -> [String]
splitWords = words

printWordCounts :: String -> IO ()
printWordCounts text = do
    let counts = countWords text
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts