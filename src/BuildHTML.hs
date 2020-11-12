module BuildHTML (genHtml) where

import Lang
import Control.Monad.Reader

type StyledText = Reader StyleDict' String

ft :: FormattedText -> String
ft (SimpleText t) = t
ft (URL u t) = "<a href=\"" ++ u ++ "\">" ++ ft t ++ "</a>"
ft (Bold b) = "<b>" ++ ft b ++ "</b>"
ft (Italic i) =  "<i>" ++ ft i ++ "</i>"

createStyle :: Style -> String
createStyle None = ""
createStyle (St s f c a) = " style=\""
                        ++ "font-size:" ++ s ++ "pt;"
                        ++ "font-family:'" ++ f ++ "';"
                        ++ "color:" ++ c ++ ";"
                        ++ "text-align:" ++ a ++ ";\""

addStyle :: StyleName -> StyledText
addStyle s = do d <- ask
                case lookup s d of
                    Nothing -> return s -- error "No existe el tipo s"
                    Just st -> return (createStyle st)

    -- usar FOLD

text :: [FormattedText] -> String
text [] = ""
text (x:xs) = ft x ++ text xs

items :: [Text] -> String
items [] = ""
items (x:xs) = "<li>" ++ text x ++"</li>\n" ++ items xs

tableRow :: [Text] -> String
tableRow [] = ""
tableRow (x:xs) = "<td>" ++ text x ++ "</td>" ++ tableRow xs

table :: [TableRow] -> StyledText
table [] = return ""
table ((Tr n cs) : ts) = do s <- addStyle n
                            rt <- table ts
                            return ("<tr" ++ s ++ ">" ++ tableRow cs ++ "</tr>" ++ rt)

sectionBody :: [Int] -> SectionBody -> StyledText
sectionBody _ (Paragraph n p) = addStyle n >>= (\s -> return ("<p" ++ s ++ ">" ++ text p ++ "</p>\n"))
sectionBody _ (Items n i) = addStyle n >>= (\s -> return ("<ul" ++ s ++ ">\n" ++ items i ++ "</ul>\n"))
sectionBody _ (Table n t) = do s <- addStyle n
                               tc <- table t
                               return ("<table" ++ s ++ ">\n" ++ tc ++ "</table>\n")
sectionBody _ (Image n i) = return ("<img src=\"" ++ i ++ "\" alt\"\">\n")
sectionBody n (Subsections s) = sections (1:n) s

chapter :: [Int] -> String
chapter [] = ""
chapter (x:xs) = show x ++ "." ++ chapter xs

title :: [Int] -> Title -> StyledText
title n (T t s) = do let p = show ((length n) + 1)
                     sty <- addStyle s
                     return ("<h"++ p ++ sty ++ ">" ++ chapter (reverse n) ++ " " ++ text t ++ "</h"++ p ++">\n")

sectionDef :: [Int] -> [SectionBody] -> StyledText
sectionDef _ [] = return ""
sectionDef n (x:xs) = do sb <- sectionBody n x
                         sd <- sectionDef n xs
                         return (sb ++ sd)
                            
section :: [Int] -> Section -> StyledText
section n (S t sb) = do tit <- title n t
                        sd <- sectionDef n sb
                        return (tit ++ sd)

sections :: [Int] -> [Section] -> StyledText
sections _ [] = return ""
sections n@(i:is) (x:xs) = do s <- section n x
                              ss <- sections ((i+1):is) xs
                              return (s ++ ss)

build :: Document -> StyledText
build (D t ss) = do t <- title [] t
                    s <- sections [1] ss
                    return ("<!DOCTYPE html>\n<body>\n" ++ t ++ s ++ "</body>" )
                            
genHtml :: Document -> StyleDict' -> String
genHtml s d = (runReader (build s)) d
