module Syntax where

data Content
  = Text String
  | Empty -- for elements like <br>
  | Element ElementType Content
  deriving (Eq, Show)

data ElementType
  = Heading Int -- h1, h2, ..., h6
  | Paragraph -- p
  | OrderedList [ElementType] -- ol
  | UnorderedList [ElementType] -- ul
  | ListItem
  | Link String -- a
  | Container -- needed??? div, section, main
  | Head
  | Body
  | BlockQuote
  | Hr -- empty
  | Br -- empty
  | Bold
  | Italic
  | Underline
  | Strikethrough
  | InlineCode
  | CodeBlock
  | Table TableType
  deriving (Eq, Show)

data TableType
  = TableHead [TableType]
  | TableBody [TableType]
  | TableRow [TableType]
  | TableCell String
  deriving (Eq, Show)

reservedMarkdownChars :: [Char] =
  [ '*',
    '~',
    '`',
    '>'
  ]
