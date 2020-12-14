{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Styles (processStyle, getStyle) where 
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

{-      Este es un intento por lanzar un error cuando un elemento se intenta estilizar con un estilo inexistente. Aún no resuelto.
data StyleError =
      StyleNotFound String
      deriving Show
      
instance MonadError StyleError (Either StyleError) where
    throwError = Left
    -}
    
getStyle :: StyleName -> StyleDict' -> Style
getStyle s d = case lookup s d of
                    Nothing -> None -- throwError $ "ADVERTENCIA: No existe el estilo "++s++". El elemento se creará sin estilo."
                    Just st -> st

processStyle :: StyleDict -> StyleDict' -> IO StyleDict'
processStyle [] d = return d
processStyle (x:xs) d = do  d' <- addStyle (fst x) (snd x) d
                            processStyle xs d'
                        
addStyle :: StyleName -> StyleParameters -> StyleDict' -> IO StyleDict'
addStyle n p d = case lookup n d of
                      Just _  -> do putStrLn $ "ADVERTENCIA: Ya existe el estilo "++n++". Toda redefinición será descartada."
                                    return d
                      Nothing -> do s <- buildStyle p d
                                    return ((n,s):d)
                                 
buildStyle :: StyleParameters -> StyleDict' -> IO Style
buildStyle params dict = case lookup "hereda" params of
                              Just n -> case lookup n dict of
                                             Nothing -> do  putStrLn $ "ADVERTENCIA: No existe el estilo "++n++"."
                                                            return None
                                             Just st -> addParams (filter (not . ((==) "hereda") . fst) params) st
                              Nothing ->                addParams params defaultStyle
                              
addParams :: StyleParameters -> Style -> IO Style
addParams [] st = return st
addParams (x:xs) st = case fst x of
                            "tamano" -> do  s <- checkNumber $ snd x
                                            let s' = case s of
                                                        Nothing -> defaultSize
                                                        Just s  -> s
                                            addParams xs (st {size = s'})
                            "fuente" -> addParams xs (st {font = snd x})
                            "color"  -> do  c <- checkColor $ snd x
                                            addParams xs (st {color = c})
                            "alineacion" -> do  a <- checkAllign $ snd x
                                                let a' = case a of
                                                            Nothing -> defaultColor
                                                            Just p  -> p
                                                addParams xs (st {allignment = a'})
                            n -> do putStrLn $ "ADVERTENCIA: No existe el parámetro "++n++". El parámetro será descartado."
                                    addParams xs st
