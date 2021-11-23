module Syntax where

data Content
  = Text String
  | Element ElementType Content
  deriving (Eq, Show)

data ElementType
  = Heading Int -- h1, h2, ..., h6
  | Paragraph -- p
  | OrderedList [ElementType] -- ol
  | UnorderedList [ElementType] -- ul
  | ListItem
  | Link -- a
  | Container -- div, section, main
  | Head
  | Body
  | BlockQuote
  | Hr
  | Br
  | Bold
  | Italic
  | Underline
  | Code
  | Table
  deriving (Eq, Show)
