{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Data.Semigroup ((<>))
import Options.Applicative
import Parse (fileParse)
import Lang
import Styles
import BuildIntHTML (genHtml)
import Yesod.Content.PDF
import Text.Blaze.Renderer.String (renderHtml)

data Opts = Opts
    { numering :: Bool
    , pageFormat :: String
    , inputFile :: !String
    , outputFile :: String
    }

optsParser :: ParserInfo Opts
optsParser =
    info
        (helper <*> programOptions)
        (fullDesc <> progDesc "Introduzca el nombre del archivo origen y el nombre del archivo salida" <>
            header
                "Generador de documentos HTML")

programOptions :: Parser Opts
programOptions =
    Opts <$> switch (long "numeracion" <> short 'n' <> help "Ennumera las páginas") <*> 
    strOption (long "formato" <> short 'f' <> value "a4" <> help "Formato de la página") <*>
    strArgument (help "Archivo de origen") <*>
    strArgument (value "test/output.html" <> help "Archivo salida")

main :: IO ()
main = do
        opts <- execParser optsParser
        args <- getArgs
        let input = inputFile opts
        let output = outputFile opts
        x <- (readFile input)
        case fileParse x of
             Left e             -> do   print e
                                        return ()
             Right (styles,doc) -> do   let styl = processStyle styles []
                                            html = genHtml doc styl
                                        pdf <- html2PDF def html
                                        let html2 = renderHtml html
                                        writeFile output html2
                                        --print (numering opts)
                                        return ()
        
