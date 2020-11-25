module BuildIntHTML () where

import Lang
import Text.Html
import Control.Monad.Reader

type StyledHtml = Reader StyleDict' Html

createStyle :: Style -> HtmlAttr
createStyle None = ""
createStyle (St s f c a) = size s 

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

table :: [TableRow] -> StyledHtml
table [] = return ""
table ((Tr n cs) : ts) = do s <- addStyle n
                            rt <- table ts
                            return ("<tr" ++ s ++ ">" ++ tableRow cs ++ "</tr>" ++ rt)
