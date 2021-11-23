module MarkdownParser where

import Data.Char (isSpace)
import Syntax (Content (E, Text), EType (..), reservedMarkdownChars)
import Text.Parsec.Token
import Text.ParserCombinators.Parsec

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
  E (Heading (length hx)) <$> lineP

ulListP :: Parser Content
ulListP = undefined

olListP :: Parser Content
olListP = undefined

quoteP :: Parser Content
quoteP = undefined

-- quoteP = Element BlockQuote <$> many (between (string ">") (string ">") textP)

codeBlockP :: Parser Content
codeBlockP = E CodeBlock <$> between (string "```") (string "```") textP

-- parses a line to handle style (bold, italics, etc), inline code
lineP :: Parser Content
lineP = boldP <|> italicP <|> strikeP <|> inlineCodeP <|> textP

boldP :: Parser Content
boldP = E Bold <$> between (string "**") (string "**") textP

italicP :: Parser Content
italicP = E Italic <$> between (char '*') (char '*') textP

strikeP :: Parser Content
strikeP = E Strikethrough <$> between (char '~') (char '~') textP

inlineCodeP :: Parser Content
inlineCodeP = E InlineCode <$> between (char '`') (char '`') textP

textP :: Parser Content
textP = Text <$> many1 (satisfy notReservedChar)
  where
    notReservedChar c = c `notElem` reservedMarkdownChars

wsP :: Parser a -> Parser a
wsP p = p <* many (satisfy isSpace)
