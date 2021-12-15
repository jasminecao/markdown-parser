{-# LANGUAGE ScopedTypeVariables #-}

module Syntax where

-- | Representation for the entire document or text file.
newtype Doc = Doc [Block]
  deriving (Eq, Show)

-- | Block element to hold line(s) of text.
data Block
  = Heading Int Line
  | Paragraph Line -- p
  | OrderedList (Int, [Block]) Int -- (start value, items) level
  | UnorderedList [Block] Int -- items level
  | Image String String -- alt src
  | BlockQuote [Line]
  | CodeBlock String
  | Hr -- empty
  | Br -- empty
  | Table TableHead TableBody -- thead, tbody
  deriving (Eq, Show)

newtype TableHead = TableHead TableRow
  deriving (Eq, Show)

newtype TableBody = TableBody [TableRow]
  deriving (Eq, Show)

newtype TableRow = TableRow [TableCell]
  deriving (Eq, Show)

newtype TableCell = TableCell Line
  deriving (Eq, Show)

-- | Line of text, may include multiple text elements.
newtype Line = Line [Text]
  deriving (Eq, Show)

instance Semigroup Line where
  (Line a) <> (Line b) = Line (a <> b)

instance Monoid Line where
  mempty = Line []

-- | Text element which holds a stylized or normal piece of text.
data Text
  = Bold String
  | Italic String
  | Strikethrough String
  | InlineCode String
  | Link [Text] String -- text, url
  | Normal String
  deriving (Eq, Show)

-- | Reserved HTML tags
htmlTags :: [String]
htmlTags =
  [ "html",
    "blockquote",
    "code",
    "pre",
    "img",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "hr",
    "br",
    "ol",
    "ul",
    "li",
    "p",
    "i",
    "b",
    "a",
    "del",
    "table",
    "tbody",
    "thead",
    "tfoot",
    "tbody",
    "td",
    "th",
    "tr"
  ]