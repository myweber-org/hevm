module WordCounter where

import Data.Char (isSpace)
import Data.List (groupBy)

countWords :: String -> Int
countWords = length . filter (not . all isSpace) . groupBy (\x y -> not (isSpace x && isSpace y))

testCountWords :: IO ()
testCountWords = do
    let testCases = [ ("", 0)
                    , ("hello", 1)
                    , ("hello world", 2)
                    , ("  hello   world  ", 2)
                    , ("multiple   spaces   between", 3)
                    , ("line\nbreak", 2)
                    , ("\t\ttabs\t\t", 1)
                    ]
    
    putStrLn "Running word counter tests:"
    mapM_ (\(input, expected) -> do
        let result = countWords input
        putStrLn $ "Input: " ++ show input ++ " -> " ++ show result
        if result == expected
            then putStrLn "  ✓ Pass"
            else putStrLn $ "  ✗ Fail (expected " ++ show expected ++ ")"
        ) testCases

main :: IO ()
main = testCountWords