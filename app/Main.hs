{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Data.Semigroup ((<>))
import Options.Applicative

import Lang
import Lib
import Styles
import Parse (fileParse)
import BuildIntHTML (genHtml)

import Text.Blaze.Renderer.String (renderHtml)
import Yesod.Content.PDF
import Data.ByteString.Char8 (writeFile)

data Opts = Opts
    { oLandscape :: Bool
    , oPageSize :: String
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
    Opts <$>
    switch (long "paisaje" <> short 'p' <> help "Orienta la página en Paisaje") <*> 
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
             Right (styles,doc) -> do   doc' <- absImgPathD input doc
                                        pageSize <- mkPageSize (oPageSize opts)
                                        let styl = processStyle styles []
                                            html = genHtml doc' styl
                                            orientation = if (oLandscape opts) then Landscape else Portrait
                                            -- ...
                                            opc = def { wkOrientation = orientation,
                                                        wkPageSize = pageSize,
                                                        wkCopies = 5
                                                }
                                        pdf <- html2PDF opc html
                                        --Prelude.writeFile (output++".html") (renderHtml html)
                                        Data.ByteString.Char8.writeFile (output++".pdf") (pdfBytes pdf)
                                        return ()
