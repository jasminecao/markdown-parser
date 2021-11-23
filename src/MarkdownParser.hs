module MarkdownParser where

import Syntax (Content (Element, Text), ElementType (Heading))
import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Data.Char (isSpace)

-- TODO: add error filepath arg to parse
parseMarkdown :: String -> Either ParseError Content
parseMarkdown = parse markdownP ""

markdownP :: Parser Content
markdownP = blockP

-- parses for a block of markdown (headings, lists, quotes, code blocks)
blockP :: Parser Content
blockP = headingP <|> listP <|> quoteP <|> codeP <|> lineP

-- parses for # heading and converts rest of line to Text
headingP :: Parser Content
headingP = do
  hx <- wsP $ many1 (char '#')
  Element (Heading (length hx)) <$> lineP

listP :: Parser Content
listP = undefined

quoteP :: Parser Content
quoteP = undefined

codeP :: Parser Content
codeP = undefined

-- parses a line to handle style (bold, italics, etc), inline code
-- TODO: handle inline syntax
lineP :: Parser Content
lineP = Text <$> many anyChar

wsP :: Parser a -> Parser a
wsP p = p <* many (satisfy isSpace)

