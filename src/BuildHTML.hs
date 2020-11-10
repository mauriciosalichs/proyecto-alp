module BuildHTML (build) where

import Lang

ft :: FormattedText -> String
ft (SimpleText t) = t
ft (URL u t) = "<a href=\"" ++ u ++ "\">" ++ ft t ++ "</a>"
ft (Bold b) = "<b>" ++ ft b ++ "</b>"
ft (Italic i) =  "<i>" ++ ft i ++ "</i>"

createStyle :: Style -> String
createStyle None = ""
createStyle (St s f c a) = "style=\""
                        ++ "font-size:" ++ s ++ "pt;"
                        ++ "font-family:'" ++ f ++ "';"
                        ++ "color:" ++ c ++ ";"
                        ++ "text-align:" ++ a ++ ";\""

addStyle :: StyleName -> StyleDict' -> String
addStyle s d = case lookup s d of
                    Nothing -> s -- error "No existe el tipo s"
                    Just st -> createStyle st

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

table :: [TableRow] -> StyleDict' -> String
table [] _ = ""
table ((Tr s cs) : ts) d = "<tr " ++ addStyle s d ++ ">" ++ tableRow cs ++ "</tr>" ++ table ts d

sectionBody :: [Int] -> SectionBody -> StyleDict' -> String
sectionBody _ (Paragraph s p) d = "<p " ++ addStyle s d ++ ">" ++ text p ++ "</p>\n"
sectionBody _ (Items s i) d = "<ul " ++ addStyle s d ++ ">\n" ++ items i ++ "</ul>\n"
sectionBody _ (Image s i) d = "<img src=\"" ++ i ++ "\" alt\"\">\n"
sectionBody _ (Table s t) d = "<table " ++ addStyle s d ++ ">\n" ++ table t d ++ "</table>\n"
sectionBody n (Subsections s) d = sections (1:n) s d

chapter :: [Int] -> String
chapter [] = ""
chapter (x:xs) = show x ++ "." ++ chapter xs

title :: [Int] -> Title -> StyleDict' -> String
title n (T t s) d = let p = show ((length n) + 1)
                    in "<h"++ p ++" " ++ addStyle s d ++ ">" ++ chapter (reverse n) ++ " " ++ text t ++ "</h"++ p ++">\n"

sectionDef :: [Int] -> [SectionBody] -> StyleDict' -> String
sectionDef _ [] _ = ""
sectionDef n (x:xs) d = sectionBody n x d ++ sectionDef n xs d
                            
section :: [Int] -> Section -> StyleDict' -> String
section n (S t sb) d = title n t d ++ sectionDef n sb d

sections :: [Int] -> [Section] -> StyleDict' -> String
sections _ [] _ = ""
sections n@(i:is) (x:xs) d = section n x d ++ sections ((i+1):is) xs d

build :: Document -> StyleDict' -> String
build (D t ss) d = "<!DOCTYPE html>\n<body>\n"
                ++ (title [] t d)
                ++ sections [1] ss d
                ++ "</body>"
