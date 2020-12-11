{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.Directory (getCurrentDirectory)
import Data.Semigroup ((<>))
import Options.Applicative
import Lang
import Styles
import Parse (fileParse)
import BuildIntHTML (genHtml)
import Text.Blaze.Renderer.String (renderHtml)
import Yesod.Content.PDF
import Data.ByteString.Char8 (writeFile)

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
             Right (styles,doc) -> do   cPath <- getCurrentDirectory
                                        let styl = processStyle styles []
                                            tPath = cPath ++ "/" ++ trimUpTo '/' (reverse input)
                                            doc' = absImgPathD tPath doc -- para convertir a pdf, es necesario que las rutas de las imagenes sean paths absolutos
                                            html = genHtml doc' styl
                                        -- agregar opciones del parser por linea de comandos y agregarlos a html2pdf
                                        pdf <- html2PDF def html
                                        Prelude.writeFile (output++".html") (renderHtml html)
                                        Data.ByteString.Char8.writeFile (output++".pdf") (pdfBytes pdf)
                                        return ()
        
absImgPathD :: String -> Document -> Document
absImgPathD p (D t s) = D t $ Prelude.map (absImgPathS p) s
    where absImgPathS p (S t sb) = S t $ Prelude.map (absImgPathSB p) sb
          absImgPathSB p (Image n s) = Image n $ p ++ s
          absImgPathSB _ sb = sb
          
trimUpTo :: Char -> String -> String
trimUpTo _ [] = []
trimUpTo c xs = if c == head xs then reverse xs else trimUpTo c $ tail xs
