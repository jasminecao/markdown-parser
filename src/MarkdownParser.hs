module MarkdownParser where

import qualified Control.Monad as Monad
import Data.Char (isSpace)
import Data.Functor
import Syntax (Block (..), Doc (Doc), Line, Text (..))
import qualified Syntax as S
import Text.Parsec.Token
import Text.ParserCombinators.Parsec as Parsec

-- for testing......
p2 :: Parser a -> String -> Either String a
p2 parser str = case parse parser "" str of
  Left err -> Left "No parses"
  Right x -> Right x

-- | Parses the complete file or text into a Doc type.
-- TODO: add error filepath arg to parse
parseMarkdown :: String -> Either ParseError Doc
parseMarkdown = parse markdownP ""

markdownP :: Parser Doc
markdownP = Doc <$> many1 blockP

-- TODO: add end of line check (\n)
-- parses for a block of markdown (headings, lists, quotes, code blocks)
blockP :: Parser Block
blockP = tryBlockP <* many (string "\n")
  where
    tryBlockP =
      try imgP
        <|> try brP
        <|> try hrP
        <|> try headingP
        <|> try ulListP
        <|> try olListP
        <|> try quoteP
        <|> try codeBlockP
        <|> paragraphP

-- parses for # heading and converts rest of line to Line
headingP :: Parser Block
headingP = do
  hx <- wsP $ many1 (char '#')
  Monad.guard (length hx < 7)
  Heading (length hx) <$> lineP

-- TODO: figure out how to implement sublists?
-- TODO: parse for `*` too
-- parses for an unordered list (- list item)
ulListP :: Parser Block
ulListP =
  UnorderedList <$> do
    wsP $ string "- " -- first hyphen must have at least one space after
    firstItem <- eolP
    remainingItems <- many $
      do
        wsP $ string "-"
        eolP
    return $ firstItem : remainingItems

-- parses for an ordered list (1. list item)
olListP :: Parser Block
olListP =
  OrderedList <$> do
    startVal <- int
    wsP (string ". ")
    firstItem <- eolP
    remainingItems <- many $
      do
        int <* wsP (string ".")
        eolP
    return (startVal, firstItem : remainingItems)

-- parses for a link ([text](link))
linkP :: Parser Text
linkP = S.Link <$> bracketsP (manyTill textP (string "]")) <*> parensP (many (noneOf ")"))

bracketsP :: Parser a -> Parser a
bracketsP p = string "[" *> p <* optional (string "]")

parensP :: Parser a -> Parser a
parensP p = string "(" *> p <* string ")"

-- parses for a img (![alt](src "title"))
imgP :: Parser Block
imgP = string "!" *> (Image <$> bracketsP (many (noneOf "]")) <*> parensP (many1 (noneOf ")")))

-- parses for a quote block (> quote)
quoteP :: Parser Block
quoteP = BlockQuote <$> many1 quoteNewLinesP
  where
    quoteNewLinesP :: Parser S.Line
    quoteNewLinesP = do
      wsP $ char '>'
      eolP

-- parses for a paragraph
paragraphP :: Parser Block
paragraphP = Paragraph <$> lineP

-- parses for a code block (```\n code \n```)
codeBlockP :: Parser Block
codeBlockP = CodeBlock <$> codeNewLinesP
  where
    codeNewLinesP :: Parser [S.Line]
    codeNewLinesP = do
      string "```\n"
      manyTill eolP (try (string "```\n"))

-- parses a string until the end of the line (\n char)
eolP :: Parser S.Line
eolP = S.Line . (: []) <$> (Normal <$> many (noneOf "\n")) <* newLineChar

-- parses for a horizontal link (---)
hrP :: Parser Block
hrP = string "---" $> Hr

-- parses for an empty line
brP :: Parser Block -- ???
brP = string "\n\n" $> Br -- Br <$> string "---"

-- parses a line of text to handle style (bold, italics, inline code, etc)
lineP :: Parser S.Line
lineP = S.Line <$> many1 textP <* char '\n'

-- parses for a text string
textP :: Parser Text
textP =
  try linkP
    <|> try italicP
    <|> try boldP
    <|> try strikeP
    <|> try inlineCodeP
    <|> try normalP

-- parses for text between a beginning and end string
betweenP :: String -> Parser String
betweenP str = between (string str) (string str) $ many1 (noneOf (str ++ "\n"))

-- parses for a bold string (**text**)
boldP :: Parser Text
boldP = Bold <$> (betweenP "**" <|> betweenP "__")

-- parses for an italic string (*text*)
italicP :: Parser Text
italicP = Italic <$> (betweenP "*" <|> betweenP "_")

-- parses for a strike through string (~~text~~)
strikeP :: Parser Text
strikeP = Strikethrough <$> betweenP "~~"

-- parses for an inline code string (`text`)
inlineCodeP :: Parser Text
inlineCodeP = InlineCode <$> betweenP "`"

-- parses for a normal, undecorated string
normalP :: Parser Text
normalP =
  try (Normal <$> stringP)
    <|> Normal <$> many1 (noneOf "\n")

-- parses for a string until a reserved character is found
-- TODO: add this to syntax?
stringP :: Parser String
stringP = many1 $ noneOf ['*', '~', '`', '>', '_', '[', ']', '\n']

-- removes trailing whitespace
wsP :: Parser a -> Parser a
wsP p = p <* many (satisfy isSpace)

-- parser to consume \n character
newLineChar :: Parser String
newLineChar = string "\n"

-- parses for an integer
int :: Parser Int
int = read <$> many1 digit