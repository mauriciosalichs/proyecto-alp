{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Data.Semigroup ((<>))
import Options.Applicative
import Lang
import Styles
import Parse (fileParse)
import BuildIntHTML (genHtml)
import Text.Blaze.Renderer.String (renderHtml)
import Yesod.Content.PDF
import Data.ByteString.Char8

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
        x <- (Prelude.readFile input)
        case fileParse x of
             Left e             -> do   print e
                                        return ()
             Right (styles,doc) -> do   let styl = processStyle styles []
                                            html = genHtml doc styl
                                        -- agregar opciones del parser por linea de comandos y agregarlos a html2pdf
                                        pdf <- html2PDF html
                                        Prelude.writeFile (output++".html") (renderHtml html)
                                        Data.ByteString.Char8.writeFile (output++".pdf") (pdfBytes pdf)
                                        return ()
        
