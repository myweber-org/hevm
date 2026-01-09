import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: WordFrequencyCounter <text>"
        (text:_) -> do
            let frequencies = countWordFrequencies text
            mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) frequencies

countWordFrequencies :: String -> [(String, Int)]
countWordFrequencies text =
    let wordsList = extractWords text
        grouped = groupWords wordsList
        sorted = sortOn (Down . snd) grouped
    in take 10 sorted

extractWords :: String -> [String]
extractWords = words . map normalizeChar
  where
    normalizeChar c
        | isAlpha c = toLower c
        | otherwise = ' '

groupWords :: [String] -> [(String, Int)]
groupWords = foldr incrementCount []
  where
    incrementCount word [] = [(word, 1)]
    incrementCount word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : incrementCount word rest