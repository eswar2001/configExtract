{-# LANGUAGE OverloadedStrings #-}

import ConfigExtract.Group
import System.Environment

main = do 
    x <- getArgs
    case x of
        [] -> run Nothing
        [x] -> run (Just x)
        _ -> error "unexpected number of arguments"