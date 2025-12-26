module WordFrequencyCounter where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Environment (getArgs)

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        grouped = foldr (\word acc -> 
            case lookup word acc of
                Just count -> (word, count + 1) : filter ((/= word) . fst) acc
                Nothing -> (word, 1) : acc
            ) [] cleanedWords
    in sortOn (Down . snd) grouped
  where
    cleanWord = filter (\c -> isAlphaNum c || c == '\'')

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

processFile :: FilePath -> IO ()
processFile filePath = do
    content <- readFile filePath
    let frequencies = countWords content
    putStrLn $ "Word frequencies for: " ++ filePath
    putStrLn $ formatOutput frequencies

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: wordfreq <file1> [file2 ...]"
        files -> mapM_ processFile files