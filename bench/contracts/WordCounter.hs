
module WordCounter where

import Data.Char (isSpace)
import Data.List (group, sort)

countWords :: String -> [(String, Int)]
countWords = map (\ws -> (head ws, length ws)) 
           . group 
           . sort 
           . words 
           . map normalize
  where
    normalize c
      | c `elem` ".,!?;:\"" = ' '
      | otherwise = toLower c

wordFrequency :: String -> IO ()
wordFrequency text = do
  let counts = countWords text
  putStrLn "Word frequencies:"
  mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) counts
  putStrLn $ "Total unique words: " ++ show (length counts)