module FibonacciMemo where

import Control.Monad.State

type FibCache = [(Int, Integer)]

fibMemo :: Int -> State FibCache Integer
fibMemo n = do
    cache <- get
    case lookup n cache of
        Just result -> return result
        Nothing -> do
            result <- case n of
                0 -> return 0
                1 -> return 1
                _ -> do
                    a <- fibMemo (n - 1)
                    b <- fibMemo (n - 2)
                    return (a + b)
            modify ((n, result):)
            return result

getFibonacci :: Int -> Integer
getFibonacci n = evalState (fibMemo n) []

main :: IO ()
main = do
    putStrLn "First 20 Fibonacci numbers:"
    mapM_ (print . getFibonacci) [0..19]