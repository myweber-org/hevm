module FibonacciMemoized where

import Data.IORef
import System.Environment

fibMemo :: IORef [(Int, Integer)] -> Int -> IO Integer
fibMemo memoRef n = do
    memo <- readIORef memoRef
    case lookup n memo of
        Just result -> return result
        Nothing -> do
            result <- if n <= 1
                then return (fromIntegral n)
                else do
                    a <- fibMemo memoRef (n - 1)
                    b <- fibMemo memoRef (n - 2)
                    return (a + b)
            modifyIORef memoRef ((n, result):)
            return result

main :: IO ()
main = do
    args <- getArgs
    case args of
        [nStr] -> do
            let n = read nStr
            memoRef <- newIORef []
            result <- fibMemo memoRef n
            putStrLn $ "Fibonacci(" ++ show n ++ ") = " ++ show result
        _ -> putStrLn "Usage: runhaskell FibonacciMemoized.hs <number>"