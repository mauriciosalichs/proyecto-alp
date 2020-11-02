module Parse (fileParse) where

import Lang
import Text.Parsec hiding (runP)
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
                          "/estilos", "/titulo", "/subtitulo",
                          "/secciones", "/items", "/tabla", "/imagen", "/espacio",
                             -- parametros de estilos
                          "tamaño", "fuente", "hereda",
                          "color", "alineacion","\n"],
         reservedOpNames = ["<",">",":","*","_","~",";","%"]
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

styleHeredits :: P StyleName
styleHeredits = (do  reserved "hereda"
                     n <- identifier
                     return n)
                <|>  return ""

styleSize :: P Size
styleSize = (do  reserved "tamaño"
                 t <- natural
                 return t)
            <|>  return 12
                
styleFont :: P Font
styleFont = (do  reserved "fuente"
                 f <- identifier
                 return f)
            <|>  return "Arial"
    
styleColor :: P Color
styleColor = (do reserved "color"
                 c <- identifier -- agregar opcion de RGB usando parens?
                 return c)
             <|> return "Negro"
    
styleAlling :: P (String, String)
styleAlling = (do reserved "alineacion"
                  h <- identifier
                  v <- identifier
                  return (h, v))
              <|> return ("Left", "Middle")

styleDef :: P Style
styleDef = do
            h <- styleHeredits
            s <- styleSize
            f <- styleFont
            c <- styleColor
            a <- styleAlling
            return (St h s f c a)
                 
styleDecls :: P [(StyleName, Style)]
styleDecls =  do  reserved "/estilos"
                  many (do  nombre <- angles $ identifier
                          --let de = (St "" 12 "" "" ("",""))
                            de <- styleDef
                            return (nombre,de))

                    
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
characters = many1 $ noneOf "\\\n~[]*_"
            
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
            reserved "\n"
            return (t,s)

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
            return (Tr s (x:xs))

table :: P SectionBody
table = do  reserved "/tabla"
            trs <- many tableRow
            return (Table trs)

image :: P SectionBody
image = do  reserved "/imagen"
            img <- many (oneOf (['a'..'z']++['A'..'Z']++['0'..'9']++":$-_.!*'(),/"))
            reserved "\n"
            return (Image img)

item :: P [FormattedText]
item = do
        reserved ":"
        i <- formattedTextL
        reserved "\n"
        return i

items :: P SectionBody
items = do  reserved "/items"
            is <- many item
            return (Items is)

vspace :: P SectionBody
vspace = do reserved "/espacio"
            n <- natural
            return (VSpace n)
            
paragraph :: P SectionBody
paragraph = do  ft <- formattedTextL
                reserved "\n"
                return (Paragraph ft)

sectionBody :: P SectionBody
sectionBody = table <|> image <|> items <|> vspace <|> paragraph

actualSection :: P (SectionBody, StyleName)
actualSection = do  s <- addStyle
                    sb <- sectionBody
                    return (sb,s)
                 
sectionDef :: Int -> P (SectionBody, StyleName)
sectionDef i = try actualSection
               <|> try (do
                            ss <- many1 (section (i+1))
                            return (Subsections ss, ""))

section :: Int -> P Section
section i = do  (repeater i sectionP)
                s <- addStyle
                t <- formattedTextL
                reserved "\n"
                sd <- many (sectionDef i)
                return ((t,s),sd)

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
               return (t,s)
              
file :: P ([(StyleName, Style)], Document)
file = do   s <- styleDecls
            d <- document
            return (s,d)
            
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

fileParse :: String -> ([(StyleName, Style)], Document)
fileParse s = case runP file s "" of
                Right t -> t
                --Left e -> []
                Left e -> ([], (([],"x"),[]))



---------------------------------------------------------------------------
---------------------------------------------------------------------------
---------------------------------------------------------------------------
---------------------------------------------------------------------------
---------------------------------------------------------------------------
---------------------------------------------------------------------------
---------------------------------------------------------------------------
---------------------------------------------------------------------------
---------------------------------------------------------------------------


{-
binding :: P (Name, STy)
binding = do v <- var
             reservedOp ":"
             ty <- typeP
             return (v, ty)
             
tyatom :: P STy
tyatom = (reserved "Nat" >> return SNatTy)
         <|> parens typeP
         <|> (do
                i <- getPos
                st <- identifier
                return (SNamedTy i st))

typeP :: P STy
typeP = try (do 
          x <- tyatom
          reservedOp "->"
          y <- typeP
          return (SFunTy x y))
      <|> tyatom
      
sconst :: P Const
sconst = CNat <$> num

unaryOpName :: P UnaryOp
unaryOpName =
      (reserved "succ" >> return Succ)
  <|> (reserved "pred" >> return Pred)
  
tyvar :: P Name
tyvar = Tok.lexeme lexer $ do
 c  <- upper
 cs <- option "" identifier
 return (c:cs)


sunaryOp :: P STerm
sunaryOp = do
             i <- getPos
             o <- unaryOpName
             return (SUnaryOp i o)

satom :: P STerm
satom =     (flip SConst <$> const <*> getPos)
       <|> flip SV <$> var <*> getPos
       <|> sunaryOp
       <|> parens stm

--para parsear una lista de binders
binders :: P [[(Name, STy)]]
binders = many (parens $ do 
                    vs <- many1 var
                    reservedOp ":"
                    ty <- typeP
                    return (map (\n -> (n, ty)) vs))
         
slam :: P STerm
slam = do i <- getPos
          reserved "fun"
          vts' <- binders
          let vts = concat vts'
          reservedOp "->"
          t <- stm
          return (SLam i vts t)

sapp :: P STerm
sapp = (do i <- getPos
           f <- satom
           args <- many satom
           return (foldl (SApp i) f args))

sifz :: P STerm
sifz = do i <- getPos
          reserved "ifz"
          c <- stm
          reserved "then"
          t <- stm
          reserved "else"
          e <- stm
          return (SIfZ i c t e)

sfix :: P STerm
sfix = do i <- getPos
          reserved "fix"
          nts <- binders
          reservedOp "->"
          t <- stm
          return (SFix i nts t)
          
sfix :: P STerm
sfix = do i <- getPos
          reserved "fix"
          (f, fty) <- parens binding
          (x, xty) <- parens binding
          reservedOp "->"
          t <- stm
          return (SFix i f fty x xty t)   

slet :: P STerm
slet = do 
     i <- getPos
     reserved "let"
     r <- ((do
            reserved "rec"
            return True) <|> return False)
     n <- var
     vts' <- binders
     let vts = concat vts'
     reservedOp ":"
     ty <- typeP
     reservedOp "="
     std <- stm
     reserved "in"
     sta <- stm
     return (SLet i n ty vts r std sta)

-- | Parser de términos azucarados
stm :: P STerm
stm = sapp <|> slet <|> slam <|> sifz <|> sfix

-- | Parser de declaraciones azucaradas
sdecll :: P (SDecl STerm)
sdecll = do i <- getPos
            reserved "let"
            r <- ((do
                     reserved "rec"
                     return True) <|> return False)
            f <- var
            nts' <- binders
            let nts = concat nts'
            reservedOp ":"
            fty <- typeP
            reservedOp "="
            t <- stm
            return (SDecl i f fty nts r t)

sdeclt :: P (SDecl STerm)
sdeclt = do i <- getPos
            reserved "type"
            st <- tyvar
            reservedOp "="
            rt <- typeP
            return (SType i st rt)

sdecl :: P (SDecl STerm)
sdecl = sdecll <|> sdeclt
        
-- | Parser de programas con azucar sintactico (listas de declaraciones) 
sprogram :: P [SDecl STerm]
sprogram = many sdecl

-- | Parsea una declaración a un término
-- Útil para las sesiones interactivas
sdeclOrSTm :: P (Either (SDecl STerm) STerm)
sdeclOrSTm =  try (Right <$> stm) <|> (Left <$> sdecl)

runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

sparse :: String -> STerm
sparse s = case runP stm s "" of
            Right t -> t
            Left e -> error ("no parse: " ++ show s)
                -}
