module Lib where

import System.Directory (getCurrentDirectory)
import Text.Read (readMaybe)

import Lang
import Yesod.Content.PDF

absImgPathD :: String -> Document -> IO Document
absImgPathD i (D t s) = do cd <- getCurrentDirectory
                           let p = trimUpTo '/' (reverse i)
                           return $ D t $ map (absImgPathS $ cd++"/"++p) s
    where absImgPathS p (S t sb) = S t $ map (absImgPathSB p) sb
          absImgPathSB p (Image n s) = Image n $ p ++ s
          absImgPathSB _ sb = sb
          
trimUpTo :: Char -> String -> String
trimUpTo _ [] = []
trimUpTo c xs = if c == head xs then reverse xs else trimUpTo c $ tail xs

mkMargin :: String -> IO UnitReal
mkMargin s = case (readMaybe s :: Maybe Double) of
                Nothing -> numberError s (Cm 1)
                Just m -> return $ Cm m

mkPageSize :: String -> IO PageSize
mkPageSize "a4" = return A4
mkPageSize "carta" = return Letter
mkPageSize s = let size = words s
               in if not $ length size == 2 then pageFormatError s else
                    let [h,w] = size
                    in case (readMaybe h :: Maybe Double) of
                            Nothing -> numberError h A4
                            Just sh -> case (readMaybe w :: Maybe Double) of
                                            Nothing -> numberError w A4
                                            Just sw -> return $ CustomPageSize (Cm sh) (Cm sw)
                    
pageFormatError :: String -> IO PageSize
pageFormatError s = do  putStrLn $ "ADVERTENCIA: "++s++" no es un formato de página válido. Se creará una página A4 por defecto."
                        return A4
                        
numberError :: String -> a -> IO a
numberError n def = do  putStrLn $ "ADVERTENCIA: "++n++" no es un número válido. Se usará la medida por defecto."
                        return def
