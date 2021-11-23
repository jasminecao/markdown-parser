{-# LANGUAGE ScopedTypeVariables #-}

module Syntax where

newtype Doc = Doc [Block]
  deriving (Eq, Show)

data Block
  = Heading Int Line
  | Paragraph Line -- p
  | OrderedList [Line]
  | UnorderedList [Line]
  | Link String Line -- a
  | BlockQuote String
  | CodeBlock String
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

-- this will be a monoid
newtype Line = Line [Text]
  deriving (Eq, Show)

data Text
  = Bold String
  | Italic String
  | Underline String
  | Strikethrough String
  | InlineCode String
  | Normal String
  deriving (Eq, Show)

reservedMarkdownChars :: [Char] =
  [ '*',
    '~',
    '`',
    '>'
  ]
