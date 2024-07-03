module Main (main) where

import System.Environment

main :: IO ()
main = do
    x <- getArgs
    if length x > 10 then putStrLn "Test suite not yet implemented." else print "HII"

data A = B Int String | C String