module Styles where 
import Lang

defaultStyle :: Style
defaultStyle = St "12" "Arial" "Black" "Left"

getStyle :: StyleName -> StyleDict' -> Style
getStyle s d = case lookup s d of
                    Nothing -> None -- error? "no existe el estilo s"
                    Just st -> st

processStyle :: StyleDict -> StyleDict' -> StyleDict'
processStyle [] d = d
processStyle (x:xs) d = let (n, p) = x
                            d' = (addStyle n p d)
                        in  processStyle xs d'
                        
addStyle :: StyleName -> StyleParameters -> StyleDict' -> StyleDict'
addStyle n p d = case lookup n d of
                      Just _  -> undefined -- error "ya existe el estilo n"
                      Nothing -> let s = buildStyle p d
                                 in ((n,s):d)
                                 
buildStyle :: StyleParameters -> StyleDict' -> Style
buildStyle params dict = case lookup "hereda" params of
                              Just n -> case lookup n dict of
                                             Nothing -> undefined -- error "no existe el estilo n"
                                             Just st -> addParams (filter (not . ((==) "hereda") . fst) params) st
                              Nothing ->                addParams params defaultStyle
                              
addParams :: StyleParameters -> Style -> Style
addParams [] st = st
addParams (x:xs) st = case fst x of -- por ahora tomamos todos como strings, sin chequear que tengan valores correctos
                            "tamano" -> addParams xs (st {size = snd x})
                            "fuente" -> addParams xs (st {font = snd x})
                            "color"  -> addParams xs (st {color = snd x})
                            "alineacion" -> addParams xs (st {allignment = snd x})
                            n -> undefined -- error "No existe el parametro n"
