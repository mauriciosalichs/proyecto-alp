module BuildIntHTML (genHtml) where

import Lang
import Text.Html as Html
import Control.Monad.Reader

type StyledHtml = Reader StyleDict' Html

createStyle :: Style -> [HtmlAttr]
createStyle None = []
createStyle (St s f c a) = [Html.size s, Html.size f, Html.color c, align a]

addStyle :: StyleName -> Html -> StyledHtml
addStyle s h = do d <- ask
                  case lookup s d of
                    Nothing -> return h -- warning "No existe el tipo s"
                    Just st -> return (Html.style (h ! (createStyle st)))

ft :: FormattedText -> Html
ft (SimpleText s) = stringToHtml s
ft (URL s t) = undefined
ft (Bold t) = bold $ ft t
ft (Italic t) = italics $ ft t

tex :: Text -> Html
tex t = foldl (+++) noHtml (fmap ft t)

items :: [Text] -> Html
items i = foldl (+++) noHtml (fmap (li.tex) i)

tableRow :: [Text] -> Html
tableRow tr = foldl (+++) noHtml (fmap (td.tex) tr)
                      
tabl :: [TableRow] -> StyledHtml
tabl [] = return noHtml
tabl ((Tr n r):ts) = do t <- addStyle n ((tr.tableRow) r)
                        rt <- tabl ts
                        return (t +++ rt)

sectionBody :: [Int] -> SectionBody -> StyledHtml
sectionBody _ (Paragraph n p) = addStyle n (paragraph $ tex p)
sectionBody _ (Items n i) = addStyle n (items i)
sectionBody _ (Table n t) = do st <- tabl t
                               addStyle n st
sectionBody _ (Image n i) = return (lineToHtml ("<img src=\"" ++ i ++ "\" alt\"\">\n"))
sectionBody n (Subsections s) = sections (1:n) s

chapter :: [Int] -> String
chapter c = foldl (++) "" (fmap show c)

title :: [Int] -> Title -> StyledHtml
title n (T t s) = do let header = case length n + 1 of
                                    1 -> h1
                                    2 -> h2
                                    3 -> h3
                                    4 -> h4
                                    5 -> h5
                                    _ -> h6
                         tc = (lineToHtml $ chapter $ reverse n) +++ tex t
                     addStyle s (header tc)

sectionDef :: [Int] -> [SectionBody] -> StyledHtml
sectionDef _ [] = return noHtml
sectionDef n (x:xs) = do sb <- sectionBody n x
                         sd <- sectionDef n xs
                         return (sb +++ sd)
                            
section :: [Int] -> Section -> StyledHtml
section n (S t sb) = do tit <- BuildIntHTML.title n t
                        sd <- sectionDef n sb
                        return (tit +++ sd)

sections :: [Int] -> [Section] -> StyledHtml
sections _ [] = return noHtml
sections n@(i:is) (x:xs) = do s <- section n x
                              ss <- sections ((i+1):is) xs
                              return (s +++ ss)

build :: Document -> StyledHtml
build (D t ss) = do t <- BuildIntHTML.title [] t
                    s <- sections [1] ss
                    return (t +++ s)
                            
genHtml :: Document -> StyleDict' -> String
genHtml s d = renderHtml $ body ((runReader (build s)) d)
