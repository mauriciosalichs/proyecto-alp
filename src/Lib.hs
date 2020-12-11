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

mkPageSize :: String -> IO PageSize
mkPageSize "a4" = return A4
mkPageSize "carta" = return Letter
mkPageSize s = let size = words s
               in if not $ length size == 2 then pageFormatError s else
                    let [h,w] = size
                    in case (readMaybe h :: Maybe Double) of
                            Nothing -> pageFormatErrorN h
                            Just sh -> case (readMaybe w :: Maybe Double) of
                                            Nothing -> pageFormatErrorN w
                                            Just sw -> return $ CustomPageSize (Cm sh) (Cm sw)
                    
pageFormatError :: String -> IO PageSize
pageFormatError s = do  putStrLn $ "ADVERTENCIA: "++s++" no es un formato de página válido."
                        return A4
                        
pageFormatErrorN :: String -> IO PageSize
pageFormatErrorN n = do putStrLn $ "ADVERTENCIA: "++n++" no es un número válido."
                        return A4
