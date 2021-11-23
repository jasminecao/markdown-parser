module MarkdownParser where

import Syntax (Content (Element, Text), ElementType (..), reservedMarkdownChars)
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
blockP = headingP <|> ulListP <|> olListP <|> quoteP <|> codeBlockP <|> lineP

-- parses for # heading and converts rest of line to Text
headingP :: Parser Content
headingP = do
  hx <- wsP $ many1 (char '#')
  Element (Heading (length hx)) <$> lineP

ulListP :: Parser Content
ulListP = undefined

olListP :: Parser Content
olListP = undefined

-- quoteP :: Parser Content
-- quoteP = Element BlockQuote <$> many (between (string ">") (string ">") textP)

codeBlockP :: Parser Content
codeBlockP = Element CodeBlock <$> between (string "```") (string "```") textP

-- parses a line to handle style (bold, italics, etc), inline code
lineP :: Parser Content
lineP = boldP <|> italicP <|> strikeP <|> inlineCodeP <|> textP

boldP :: Parser Content
boldP = Element Bold <$> between (string "**") (string "**") textP

italicP :: Parser Content
italicP = Element Italic <$> between (char '*') (char '*') textP

strikeP :: Parser Content
strikeP = Element Strikethrough <$> between (char '~') (char '~') textP

inlineCodeP :: Parser Content
inlineCodeP = Element InlineCode <$> between (char '`') (char '`') textP

textP :: Parser Content
textP = Text <$> many1 (satisfy notReservedChar)
  where
    notReservedChar c = c `notElem` reservedMarkdownChars

wsP :: Parser a -> Parser a
wsP p = p <* many (satisfy isSpace)

