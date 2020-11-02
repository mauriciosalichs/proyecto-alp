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
    | St { heredits :: StyleName, size :: Size, font :: Font, color :: Color, allignment :: (String, String) }

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
    Paragraph Text
  | Table Tbl
  | Image Img
  | Items [Text]
  | VSpace Space
  | Subsections [Section]


-- tipos principales. modificar para no usar tantas tuplas en esta parte

type Title = (Text, StyleName)
type Section = (Title, [(SectionBody, StyleName)])
type Document = (Title, [Section])
