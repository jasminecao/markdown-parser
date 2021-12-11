{-# LANGUAGE ScopedTypeVariables #-}

module Syntax where

-- | Representation for the entire document or text file.
newtype Doc = Doc [Block]
  deriving (Eq, Show)

-- | Block element to hold line(s) of text.
data Block
  = Heading Int Line
  | Paragraph Line -- p
  | OrderedList (Int, [Line]) -- start value, items
  | UnorderedList [Line]
  | Link Line String -- a
  | Image String String String -- alt src title
  | BlockQuote [Line]
  | CodeBlock [Line]
  | Hr -- empty
  | Br -- empty
  | Table TableType
  deriving (Eq, Show)

data TableType
  = TableHead [TableType]
  | TableBody [TableType]
  | TableRow [TableType]
  | TableCell Line
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
  | Normal String
  deriving (Eq, Show)
