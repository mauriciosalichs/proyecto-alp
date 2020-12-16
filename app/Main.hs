{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Data.Semigroup ((<>))
import Options.Applicative

import Lang
import Lib
import Styles (processStyles)
import Parse (fileParse)
import BuildIntHTML (genHtml)

import Text.Blaze.Renderer.String (renderHtml)
import Yesod.Content.PDF
import Data.ByteString.Char8 (writeFile)

data Opts = Opts
    { oLandscape :: Bool
    , oTitleBreak :: Bool
    , oPageSize :: String
    , oVMargin :: String
    , oHMargin :: String
    , inputFile :: !String
    , outputFile :: String
    }

optsParser :: ParserInfo Opts
optsParser =
    info
        (helper <*> programOptions)
        (fullDesc <> progDesc "Introduzca el nombre del archivo origen y (opcionlamente) el nombre del archivo salida." <>
            header
                "Generador de documentos HTML.")

programOptions :: Parser Opts
programOptions =
    Opts <$>
    switch (long "paisaje" <> short 'p' <> help "Orienta la página en formato paisaje.") <*> 
    switch (long "titulo" <> short 't' <> help "La primer página es solo el título.") <*> 
    strOption (long "formato" <> short 'f' <> value "a4" <> help "Formato de la página. Opciones posibles: \"a4\", \"carta\" o configurar manualmente con el formato 'largo ancho' (en cm).") <*>
    strOption (long "margenv" <> value "1" <> help "Margenes verticales (en cm).") <*>
    strOption (long "margenh" <> value "1" <> help "Margenes horizontales (en cm).") <*>
    strArgument (help "Archivo de origen") <*>
    strArgument (value "output" <> help "Archivo salida.")

main :: IO ()
main = do
        opts <- execParser optsParser
        args <- getArgs
        let input = inputFile opts
        let output = outputFile opts
        x <- (Prelude.readFile input)
        case fileParse x of
             Left e             -> do   print e -- Error de parseo
                                        return ()
             Right (styles,doc) -> do   doc' <- absImgPathD input doc
                                        pageSize <- mkPageSize (oPageSize opts)
                                        vMargin <- mkMargin (oVMargin opts)
                                        hMargin <- mkMargin (oHMargin opts)
                                        case processStyles styles [] of
                                             Left e -> do   putStrLn e -- Error de conversión de estilos
                                                            return ()
                                             Right st -> do let tb = (oTitleBreak opts)
                                                                orientation = if (oLandscape opts) then Landscape else Portrait
                                                                html = genHtml doc' st tb
                                                            
                                                                opc = def { wkOrientation = orientation,
                                                                            wkPageSize = pageSize,
                                                                            wkMarginBottom = vMargin,
                                                                            wkMarginTop = vMargin,
                                                                            wkMarginLeft = hMargin,
                                                                            wkMarginRight = hMargin
                                                                    }

                                                            pdf <- html2PDF opc html
                                                            --Prelude.writeFile (output++".html") (renderHtml html)
                                                            Data.ByteString.Char8.writeFile (output++".pdf") (pdfBytes pdf)
                                                            return ()
