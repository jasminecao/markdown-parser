module MarkdownParser where

import qualified Control.Monad as Monad
import Data.Char (isSpace)
import Syntax (Block (..), Doc (Doc), Line, Text (..), reservedMarkdownChars)
import qualified Syntax as S
import Text.Parsec.Token
import Text.ParserCombinators.Parsec as Parsec

-- | Parses the complete file or text into a Doc type.
-- TODO: add error filepath arg to parse
parseMarkdown :: String -> Either ParseError Doc
parseMarkdown = parse markdownP ""

markdownP :: Parser Doc
markdownP = Doc <$> many blockP

-- TODO: add end of line check (\n)
-- parses for a block of markdown (headings, lists, quotes, code blocks)
blockP :: Parser Block
blockP = headingP <|> ulListP <|> olListP <|> quoteP <|> codeBlockP <|> paragraphP

-- parses for # heading and converts rest of line to Line
headingP :: Parser Block
headingP = do
  hx <- wsP $ many1 (char '#')
  Monad.guard (length hx < 7)
  Heading (length hx) <$> lineP

-- parses for an unordered list (- list item)
ulListP :: Parser Block
ulListP =
  UnorderedList
    <$> many (string "- " *> lineP)

-- parses for an ordered list (1. list item)
olListP :: Parser Block
olListP = OrderedList <$> many (int *> char '.' *> many1 space *> lineP)

-- parses for a link ([text](link))
linkP :: Parser Block
linkP = undefined -- (Link <$> brackets stringP) <$> parens lineP

-- parses for a quote block (> quote)
quoteP :: Parser Block
quoteP = BlockQuote <$> (concat <$> many1 (string ">" *> stringP))

-- parses for a paragraph
paragraphP :: Parser Block
paragraphP = Paragraph <$> lineP

-- parses for a code block (```code```)
codeBlockP :: Parser Block
codeBlockP = CodeBlock <$> between (string "```") (string "```") stringP

-- parses for a horizontal link (---)
hrP :: Parser Block
hrP = string "---" *> pure Hr

-- parses for an empty line
brP :: Parser Block -- ???
brP = undefined -- Br <$> string "---"

-- parses a line of text to handle style (bold, italics, inline code, etc)
lineP :: Parser S.Line
lineP = S.Line <$> many textP

-- parses for a text string
textP :: Parser Text
textP = boldP <|> italicP <|> strikeP <|> inlineCodeP <|> normalP

-- parses for a bold string (**text**)
-- TODO: also add underscore parsing __text__
boldP :: Parser Text
boldP = Bold <$> between (string "**") (string "**") stringP

-- parses for an italic string (*text*)
-- TODO: also add underscore parsing _text_
italicP :: Parser Text
italicP = Italic <$> between (char '*') (char '*') stringP

-- parses for a strike through string (~~text~~)
strikeP :: Parser Text
strikeP = Strikethrough <$> between (string "~~") (string "~~") stringP

-- parses for an inline code string (`text`)
inlineCodeP :: Parser Text
inlineCodeP = InlineCode <$> between (char '`') (char '`') stringP

-- parses for a normal, undecorated string
normalP :: Parser Text
normalP = Normal <$> stringP

-- parses for a string until a reserved character is found
stringP :: Parser String
stringP = many1 (satisfy notReservedChar)
  where
    notReservedChar c = c `notElem` reservedMarkdownChars

-- removes trailing whitespace
wsP :: Parser a -> Parser a
wsP p = p <* many (satisfy isSpace)

-- parses for an integer
int :: Parser Int
int = read <$> ((++) <$> string "-" <*> many1 digit <|> many1 digit)