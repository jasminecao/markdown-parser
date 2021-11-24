module MarkdownParser where

import Data.Char (isSpace)
import Syntax (Block (..), Doc (Doc), Line, Text (..), reservedMarkdownChars)
import qualified Syntax as S
import qualified Control.Monad as Monad
import Text.Parsec.Token
import Text.ParserCombinators.Parsec as Parsec

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
  Monad.guard (length hx < 7)
  Heading (length hx) <$> lineP

ulListP :: Parser Block
ulListP = UnorderedList <$> 
  many (string "- " *> lineP)

olListP :: Parser Block
olListP = OrderedList <$> many (int *> char '.' *> many1 space *> lineP)

linkP :: Parser Block
linkP = undefined -- (Link <$> brackets stringP) <$> parens lineP

quoteP :: Parser Block
quoteP = BlockQuote <$> (concat <$> many1 (string ">" *> stringP))

paragraphP :: Parser Block
paragraphP = Paragraph <$> lineP

codeBlockP :: Parser Block
codeBlockP = CodeBlock <$> between (string "```") (string "```") stringP

hrP :: Parser Block
hrP = string "---" *> pure Hr

brP :: Parser Block -- ???
brP = undefined -- Br <$> string "---"

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

int :: Parser Int
int = read <$> ((++) <$> string "-" <*> many1 digit <|> many1 digit)