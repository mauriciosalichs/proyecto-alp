module Lang where

type StyleName = String
type Size = Integer
type Space = Integer
type Font = String
type Img = String
type Color = String -- ver si puede ser RGB (Int, Int, Int)

data HAllignment = ALeft | ARight | ACenter | AJustified
data VAllignment = ATop | AMiddle | ABottom
type Allignment = (HAllignment, VAllignment)
  
-- | Tipo de Estilos
data Style = 
      None  
    | St { size :: Size, font :: Font, color :: Color, allignment :: (String, String) }

type StyleParameters = [(String,String)]
type StyleDict = [(StyleName, StyleParameters)]

data FormattedText = 
    SimpleText String
  | URL String FormattedText
  | Bold FormattedText
  | Italic FormattedText

type Text = [FormattedText]

data TableRow = 
      Tr { style :: StyleName, columns :: [Text] }
    
type Tbl = [TableRow]
  
data SectionBody =
    Paragraph StyleName Text
  | Table StyleName Tbl
  | Image StyleName Img
  | Items StyleName [Text]
  | VSpace Space
  | Subsections [Section]

data Title = T { ttext :: Text, tstyle :: StyleName }
data Section = S { stitle :: Title, sbody :: [SectionBody] }
data Document = D { dtitle :: Title, dbody :: [Section] }
