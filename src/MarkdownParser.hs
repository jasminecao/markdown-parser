module MarkdownParser where

import Data.Char (isSpace)
import Syntax (Block (..), Doc (Doc), Line, Text (..), reservedMarkdownChars)
import qualified Syntax as S
import Text.Parsec.Token
import Text.ParserCombinators.Parsec

-- TODO: add error filepath arg to parse
parseMarkdown :: String -> Either ParseError Doc
parseMarkdown = parse markdownP ""

markdownP :: Parser Doc
markdownP = Doc <$> many blockP

-- TODO: add end of line check (\n)
-- parses for a block of markdown (headings, lists, quotes, code blocks)
blockP :: Parser Block
blockP = headingP <|> ulListP <|> olListP <|> quoteP <|> codeBlockP <|> paragraphP

-- parses for # heading and converts rest of line to Text
headingP :: Parser Block
headingP = do
  hx <- wsP $ many1 (char '#')
  Heading (length hx) <$> lineP

ulListP :: Parser Block
ulListP = undefined

olListP :: Parser Block
olListP = undefined

quoteP :: Parser Block
quoteP = undefined

-- quoteP = Element BlockQuote <$> many (between (string ">") (string ">") textP)

paragraphP :: Parser Block
paragraphP = undefined

codeBlockP :: Parser Block
codeBlockP = CodeBlock <$> between (string "```") (string "```") stringP

-- parses a line to handle style (bold, italics, etc), inline code
lineP :: Parser S.Line
lineP = S.Line <$> many textP

textP :: Parser Text
textP = boldP <|> italicP <|> strikeP <|> inlineCodeP <|> normalP

boldP :: Parser Text
boldP = Bold <$> between (string "**") (string "**") stringP

italicP :: Parser Text
italicP = Italic <$> between (char '*') (char '*') stringP

strikeP :: Parser Text
strikeP = Strikethrough <$> between (char '~') (char '~') stringP

inlineCodeP :: Parser Text
inlineCodeP = InlineCode <$> between (char '`') (char '`') stringP

normalP :: Parser Text
normalP = Normal <$> stringP

stringP :: Parser String
stringP = many1 (satisfy notReservedChar)
  where
    notReservedChar c = c `notElem` reservedMarkdownChars

wsP :: Parser a -> Parser a
wsP p = p <* many (satisfy isSpace)
