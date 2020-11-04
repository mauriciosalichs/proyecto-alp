module Parse (fileParse) where

import Lang
import Text.Parsec hiding (runP)
--import Text.Parsec.Char
import Data.Char ( isNumber, ord )
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language ( GenLanguageDef(..), emptyDef )

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------

-- | Analizador de Tokens
lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser $
        emptyDef {
         commentLine    = "#",
        reservedNames = [    -- comandos
                          "/estilos", "/titulo", "/subtitulo","/secciones", "/items", "/tabla", "/imagen", "/espacio","\n"],
         reservedOpNames = ["<",">",":","*","_","~",";"]
        }

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

natural :: P Integer
natural = Tok.natural lexer

parens :: P a -> P a
parens = Tok.parens lexer

braces :: P a -> P a
braces = Tok.braces lexer

angles :: P a -> P a
angles = Tok.angles lexer

brackets :: P a -> P a
brackets = Tok.brackets lexer

identifier :: P String
identifier = Tok.identifier lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

-----------------------
-- Parser de estilos
-----------------------

styleDescription :: P String
styleDescription = many1 (oneOf (['a'..'z']++['A'..'Z']++['0'..'9']))

styleParameter :: P String
styleParameter = many1 (oneOf (['a'..'z']++['A'..'Z']))

styleDef' :: P (String,String)
styleDef' = do name <- styleParameter
               whiteSpace
               desc <- styleDescription
               oneOf "\n"
               return (name,desc)

styleDef :: P StyleParameters
styleDef = many styleDef'
                 
styleDecls :: P StyleDict
styleDecls =  do  reserved "/estilos"
                  many (do  nombre <- angles $ identifier
                            params <- styleDef
                            many (oneOf "\n")
                            return (nombre,params) )

                    
-----------------------
-- Parsers auxiliares
-----------------------

addStyle :: P StyleName
addStyle = (do nombre <- angles $ identifier
               return nombre)
           <|> return ""

url :: P String
url = many (oneOf (['a'..'z']++['A'..'Z']++['0'..'9']++":$-_.!*'(),/"))
           
link :: P FormattedText
link = brackets $ do
                    u <- url
                    whiteSpace
                    t <- formattedText
                    return (URL u t)

bold :: P FormattedText
bold = do   oneOf "*"
            t <- formattedText
            oneOf "*"
            return (Bold t)
      
italic :: P FormattedText
italic = do oneOf "_"
            t <- formattedText
            oneOf "_"
            return (Italic t)

characters :: P String
characters = many1 $ noneOf "\\\n~[]*_;"
            
simpleText :: P FormattedText
simpleText = do
                t <- characters
                return (SimpleText t)
                
formattedText :: P FormattedText
formattedText = link <|> bold <|> italic <|> simpleText

formattedTextL :: P [FormattedText]
formattedTextL = many1 formattedText

-----------------------
-- Parser de titulos
-----------------------

title :: P Title
title = do  reserved "/titulo"
            s <- addStyle
            t <- formattedTextL
            many (oneOf "\n")
            return (T t s)

-----------------------
-- Paser de secciones
-----------------------

sectionP :: P ()
sectionP = reserved "~"

repeater :: Int -> P () -> P ()
repeater 0 p = notFollowedBy p
repeater n p = p >> repeater (n-1) p

tableRow :: P TableRow
tableRow = do
            reserved ":"
            s <- addStyle
            x <- formattedTextL
            xs <- many (do  reserved ";"
                            x' <- formattedTextL
                            return x')
            many (oneOf "\n")
            return (Tr s (x:xs))

table :: P SectionBody
table = do  reserved "/tabla"
            s <- addStyle
            trs <- many tableRow
            return (Table s trs)

image :: P SectionBody
image = do  reserved "/imagen"
            s <- addStyle
            img <- many (oneOf (['a'..'z']++['A'..'Z']++['0'..'9']++":$-_.!*'(),/"))
            many (oneOf "\n")
            return (Image s img)

item :: P [FormattedText]
item = do
        reserved ":"
        i <- formattedTextL
        many (oneOf "\n")
        return i

items :: P SectionBody
items = do  reserved "/items"
            s <- addStyle
            is <- many item
            return (Items s is)

vspace :: P SectionBody
vspace = do reserved "/espacio"
            n <- natural
            return (VSpace n)
            
paragraph :: P SectionBody
paragraph = do  s <- addStyle
                ft <- formattedTextL
                many (oneOf "\n")
                return (Paragraph s ft)

sectionBody :: P SectionBody
sectionBody = table <|> image <|> items <|> vspace <|> paragraph
                 
sectionDef :: Int -> P SectionBody
sectionDef i = try sectionBody
               <|> try (do
                            ss <- many1 (section (i+1))
                            return (Subsections ss))

section :: Int -> P Section
section i = do  (repeater i sectionP)
                s <- addStyle
                t <- formattedTextL
                many (oneOf "\n")
                sd <- many (sectionDef i)
                return (S (T t s) sd)

sections :: P [Section]
sections = do
                reserved "/secciones"
                s <- many (section 1)
                return s

-----------------------
-- Paser final
-----------------------                

document :: P Document
document = do  t <- title
               s <- sections
               return (D t s)
              
file :: P (StyleDict, Document)
file = do   s <- styleDecls
            d <- document
            return (s,d)
            
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

fileParse :: String -> Either ParseError (StyleDict, Document)
fileParse s = runP file s ""
