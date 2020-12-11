{-# LANGUAGE OverloadedStrings #-}

module BuildIntHTML (genHtml) where

import Lang
import Data.String
import Text.Blaze.Internal
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Control.Monad.Reader

type StyledHtml = ReaderT StyleDict' MarkupM ()

createStyle :: Style -> String
createStyle None = ""
createStyle (St s f c a) = "font-size:" ++ s ++ "pt;"
                        ++ "font-family:'" ++ f ++ "';"
                        ++ "color:" ++ c ++ ";"
                        ++ "text-align:" ++ a ++ ";"

addStyle :: StyleName -> Html -> StyledHtml
addStyle s h = do d <- ask
                  case lookup s d of
                    Nothing -> lift h -- warning "No existe el tipo s"
                    Just st -> lift $ h ! (A.style $ fromString $ createStyle st)

ft :: FormattedText -> Html
ft (SimpleText s) = fromString s
ft (URL s t) = (a $ ft t) ! (href $ fromString s)
ft (Bold t) = b $ ft t
ft (Italic t) = i $ ft t

tex :: Text -> Html
tex [] = fromString ""
tex (t:ts) = do ft t
                tex ts

items :: [Text] -> Html
items [] = fromString ""
items (i:is) = do li $ tex i
                  items is

tableRow :: [Text] -> Html
tableRow [] = fromString ""
tableRow (tr:trs) = do td $ tex tr
                       tableRow trs
                      
--tabl :: [TableRow] -> StyledHtml
--tabl [] = lift $ fromString ""
--tabl ((Tr n r):ts) = do addStyle n (tr $ tableRow r)
--                        tabl ts

-- por ahora ignoramos el estilo de cada fila
tabl :: [TableRow] -> Html
tabl [] = fromString ""
tabl ((Tr _ r):ts) = do tr $ tableRow r
                        tabl ts

sectionBody :: [Int] -> SectionBody -> StyledHtml
sectionBody _ (Paragraph n t) = addStyle n (p $ tex t)
sectionBody _ (Items n i) = addStyle n $ items i
sectionBody _ (Table n t) = addStyle n $ table $ tabl t
sectionBody _ (Image _ i) = lift $ img ! (src $ fromString i) -- por ahora ignoramos el estilo
sectionBody n (Subsections s) = sections (1:n) s

chapter :: [Int] -> String
chapter c = foldr (++) " " (fmap (\x -> show x ++ ".") c)

title :: [Int] -> Title -> StyledHtml
title n (T t s) = do let header = case length n + 1 of
                                    1 -> h1
                                    2 -> h2
                                    3 -> h3
                                    4 -> h4
                                    5 -> h5
                                    _ -> h6
                         tc = header $ do fromString (chapter $ reverse n)
                                          tex t
                     lift tc
                     
sectionDef :: [Int] -> [SectionBody] -> StyledHtml
sectionDef _ [] = lift $ fromString ""
sectionDef n (x:xs) = do sectionBody n x
                         sectionDef n xs
                            
doSection :: [Int] -> Section -> StyledHtml
doSection n (S t sb) = do BuildIntHTML.title n t
                          sectionDef n sb

sections :: [Int] -> [Section] -> StyledHtml
sections _ [] = lift $ fromString ""
sections n@(i:is) (x:xs) = do doSection n x
                              sections ((i+1):is) xs

build :: Document -> StyledHtml
build (D t ss) = do BuildIntHTML.title [] t
                    sections [1] ss

genHtml :: Document -> StyleDict' -> Html
genHtml s d = docTypeHtml $ body (runReaderT (build s) d)
