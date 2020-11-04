module BuildHTML (build) where

import Lang

ft :: FormattedText -> String
ft (SimpleText t) = t
ft (URL u t) = "<a href=\"" ++ u ++ "\">" ++ ft t ++ "</a>"
ft (Bold b) = "<b>" ++ ft b ++ "</b>"
ft (Italic i) =  "<i>" ++ ft i ++ "</i>"


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

table :: [TableRow] -> String
table [] = ""
table ((Tr _ cs) : ts) = "<tr>" ++ tableRow cs ++ "</tr>" ++ table ts

sectionBody :: [Int] -> SectionBody -> String
sectionBody _ (Paragraph _ p) = "<p>" ++ text p ++ "</p>\n"
sectionBody _ (Items _ i) = "<ul>\n" ++ items i ++ "</ul>\n"
sectionBody _ (Image _ i) = "<img src=\"" ++ i ++ "\" alt\"\">\n"
sectionBody _ (Table _ t) = "<table>\n" ++ table t ++ "</table>\n"
sectionBody n (Subsections s) = sections (1:n) s

chapter :: [Int] -> String
chapter [] = ""
chapter (x:xs) = show x ++ "." ++ chapter xs

title :: [Int] -> Title -> String
title n (T t _) = let p = show ((length n) + 1)
                 in "<h"++ p ++">" ++ chapter (reverse n) ++ " " ++ text t ++ "</h"++ p ++">\n"

sectionDef :: [Int] -> [SectionBody] -> String
sectionDef _ [] = ""
sectionDef n (x:xs) = sectionBody n x ++ sectionDef n xs
                            
section :: [Int] -> Section -> String
section n (S t sb) = title n t ++ sectionDef n sb

sections :: [Int] -> [Section] -> String
sections _ [] = ""
sections n@(i:is) (x:xs) = section n x ++ sections ((i+1):is) xs

build :: Document -> String
build (D t ss) =  "<!DOCTYPE html>\n<body>\n"
                ++ (title [] t)
                ++ sections [1] ss
                ++ "</body>"
