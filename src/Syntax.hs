{-# LANGUAGE ScopedTypeVariables #-}

module Syntax where

newtype Doc = Doc [E]
  deriving (Eq, Show)

data E = 
    Heading Int Block
  | Paragraph Block -- p
  | OrderedList [Block]
  | UnorderedList [Block]
  | Link String Block -- a
  | Container [E] -- needed??? div, section, main
  | Head E
  | Body E
  | BlockQuote String
  | CodeBlock String
  | Hr -- empty
  | Br -- empty
  | Table TableType
  deriving (Eq, Show)

-- this will be a monoid
newtype Block = Block [Text]
  deriving (Eq, Show)

data Text =
    Bold String
  | Italic String
  | Underline String
  | Strikethrough String
  | InlineCode String
  | Normal String
  deriving (Eq, Show)

data TableType
  = TableHead [TableType]
  | TableBody [TableType]
  | TableRow [TableType]
  | TableCell Block
  deriving (Eq, Show)

reservedMarkdownChars :: [Char] =
  [ '*',
    '~',
    '`',
    '>'
  ]
