module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Environment (getArgs)

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let words' = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        freqMap = foldr (\w m -> case lookup w m of
                                  Just count -> (w, count + 1) : filter ((/= w) . fst) m
                                  Nothing -> (w, 1) : m) [] words'
    in sortOn (Down . snd) freqMap

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

processFile :: FilePath -> IO ()
processFile filename = do
    content <- readFile filename
    putStrLn $ formatOutput $ countWords content

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> processFile filename
        _ -> putStrLn "Usage: wordfreq <filename>"