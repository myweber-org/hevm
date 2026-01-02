module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Environment (getArgs)

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map toLower wordsList
        grouped = foldr countHelper [] cleaned
    in sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

formatOutput :: [WordCount] -> String
formatOutput counts = 
    unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

processFile :: FilePath -> IO ()
processFile filename = do
    content <- readFile filename
    let frequencies = countWords content
    putStrLn $ formatOutput frequencies

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> processFile filename
        _ -> putStrLn "Usage: wordfrequency <filename>"