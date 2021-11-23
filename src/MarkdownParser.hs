module MarkdownParser where

import Syntax (Content (Element, Text), ElementType (Heading), reservedMarkdownChars)
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
blockP = headingP <|> listP <|> quoteP <|> codeBlockP <|> lineP

-- parses for # heading and converts rest of line to Text
headingP :: Parser Content
headingP = do
  hx <- wsP $ many1 (char '#')
  Element (Heading (length hx)) <$> lineP

listP :: Parser Content
listP = undefined

quoteP :: Parser Content
quoteP = undefined

codeBlockP :: Parser Content
codeBlockP = undefined

-- parses a line to handle style (bold, italics, etc), inline code
-- TODO: handle inline syntax
lineP :: Parser Content
lineP = Text <$> many anyChar

boldP :: Parser Content
boldP = undefined -- between (char '*') (char '*') textP

italicP :: Parser Content
italicP = between (char '*') (char '*') textP

strikeP :: Parser Content
strikeP = undefined

codeP :: Parser Content
codeP = undefined

textP :: Parser Content
textP = Text <$> many1 (satisfy notReservedChar)
  where
    notReservedChar c = c `notElem` reservedMarkdownChars

wsP :: Parser a -> Parser a
wsP p = p <* many (satisfy isSpace)

