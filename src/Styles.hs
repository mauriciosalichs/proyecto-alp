{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Styles (processStyles, getStyle) where 
import Lang
import ParameterChecker
--import Control.Monad.Error

defaultSize :: Size
defaultSize = "12"

defaultFont :: Font
defaultFont = "arial"

defaultColor :: Color
defaultColor = "black"

defaultAllignment :: Allignment
defaultAllignment = "left"

defaultStyle :: Style
defaultStyle = St defaultSize defaultFont defaultColor defaultAllignment

{-
data StyleError =
            ExistingStyle String
        |   UnexistingStyle String
        |   UnexistingParam String

instance Show StyleError where
    show (ExistingStyle s) = "ERROR: Ya existe el estilo "++s++"."
    show (UnexistingStyle s) = "ERROR: No existe el estilo "++s++"."
    show (UnexistingParam p) = "ERROR: No existe el parámetro "++p++"."
      
instance MonadError StyleError (Either StyleError) where
    throwError = Left
    -}   

getStyle :: StyleName -> StyleDict' -> Style
getStyle s d = case lookup s d of
                    Nothing -> None -- throwError $ "ADVERTENCIA: No existe el estilo "++s++". El elemento se creará sin estilo."
                    Just st -> st

processStyles :: StyleDict -> StyleDict' -> Either String StyleDict'
processStyles [] d = return d
processStyles (x:xs) d = do case addStyle (fst x) (snd x) d of
                                Right d' -> processStyles xs d'
                                e -> e
                        
addStyle :: StyleName -> StyleParameters -> StyleDict' -> Either String StyleDict'
addStyle n p d = case lookup n d of
                      Just _  -> Left $ "ERROR: Ya existe el estilo "++n++"."
                      Nothing -> case buildStyle p d of
                                      Right s -> Right ((n,s):d)
                                      Left e -> Left e
                                 
buildStyle :: StyleParameters -> StyleDict' -> Either String Style
buildStyle params dict = case lookup "hereda" params of
                              Just n -> case lookup n dict of
                                             Nothing -> Left $ "ERROR: No existe el estilo "++n++"."
                                             Just st -> addParams (filter (not . ((==) "hereda") . fst) params) st
                              Nothing ->                addParams params defaultStyle
                              
addParams :: StyleParameters -> Style -> Either String Style
addParams [] st = Right st
addParams (x:xs) st = case fst x of
                            "tamano" -> case checkNumber $ snd x of
                                             Right s -> addParams xs (st {size = s})
                                             Left e -> Left e
                            "fuente" -> addParams xs (st {font = snd x})
                            "color"  -> case checkColor $ snd x of
                                             Right c -> addParams xs (st {color = c})
                                             Left e -> Left e
                            "alineacion" -> case checkAllign $ snd x of
                                                 Right a  -> addParams xs (st {allignment = a})
                                                 Left e -> Left e
                            p -> Left $ "ERROR: No existe el parámetro "++p++"."
