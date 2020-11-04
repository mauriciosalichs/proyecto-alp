module Main where

import System.Environment (getArgs)
import Parse (fileParse)
import Lang
import BuildHTML (build)

main :: IO ()
main = do
        args <- getArgs
        -- assert (length args == 2)        buscar alguna estructura monádica para esto
        let input = head args
        let output = last args
        x <- (readFile input)
        case fileParse x of
             Left e             -> do   print e
                                        return ()
             Right (styles,doc) -> do   let html = build doc
                                        writeFile output html
                                        return ()
        
