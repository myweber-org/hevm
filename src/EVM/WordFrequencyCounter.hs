module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

countWords :: String -> [WordFreq]
countWords text = 
    let wordsList = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        grouped = group $ sort wordsList
        freqs = map (\ws -> (head ws, length ws)) grouped
    in sortOn (Down . snd) freqs

printHistogram :: [WordFreq] -> IO ()
printHistogram freqs = 
    let maxFreq = maximum (map snd freqs)
        scale = 50
        scaleFactor = if maxFreq == 0 then 0 else scale `div` maxFreq
    in mapM_ (\(word, count) -> 
        putStrLn $ word ++ ": " ++ replicate (count * scaleFactor) '█' ++ " " ++ show count) freqs

analyzeText :: String -> IO ()
analyzeText text = do
    putStrLn "Word Frequency Analysis:"
    putStrLn "========================="
    let freqs = countWords text
    printHistogram $ take 20 freqs
    putStrLn $ "\nTotal unique words: " ++ show (length freqs)
    putStrLn $ "Most frequent word: " ++ fst (head freqs) ++ " (" ++ show (snd (head freqs)) ++ " occurrences)"