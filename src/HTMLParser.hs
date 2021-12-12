module HTMLParser where

import qualified Control.Monad as Monad
import Data.Char (isSpace)
import Syntax (Block (..), Doc (Doc), Line, Text (..))
import qualified Syntax as S
import Text.Parsec.Token
import Text.ParserCombinators.Parsec

-- for testing (again LOL)
p3 :: Parser a -> String -> Either String a
p3 parser str = case parse parser "" str of
  Left err -> Left "No parses"
  Right x -> Right x

htmlReserved :: Char -> Bool
htmlReserved c = c `elem` ['/', '<', '>']

text :: Parser String
text = many1 $ satisfy (not . htmlReserved)

openingTag :: String -> Parser String
openingTag tag = string $ '<' : tag ++ ">"

closingTag :: String -> Parser String
closingTag tag = string $ '<' : '/' : tag ++ ">"

-- Parser between tags
container :: String -> Parser a -> Parser a
container tag p = openingTag tag *> p <* closingTag tag
-- container tag p = if x == tag && z == tag then y else Left "no parses" where
--                 (x,y,z) = (,,) <$> openingTag
--                             <*> p
--                             <*> closingTag

{-parseHtml :: String -> Either ParseError Doc
parseHtml = parse htmlP ""

htmlP :: Parser Doc
htmlP = Doc <$> many1 hBlockP

-- TODO: add end of line check (\n)
hBlockP :: Parser Block
hBlockP = tryBlockP <* many (string "\n")
  where
    tryBlockP =
      try hImgP
        <|> try hBrP
        <|> try hHrP
        <|> try hHeadingP
        <|> try hUListP
        <|> try hOListP
        <|> try hQuoteP
        <|> try hCodeBlockP
        <|> hParagraphP

-- parses for # heading and converts rest of line to Line
hHeadingP :: Parser Block
hHeadingP = do
  hx <- wsP $ many1 (char '#')
  Monad.guard (length hx < 7)
  Heading (length hx) <$> lineP

-- TODO: figure out how to implement sublists?
-- parses for an unordered list (- list item)
ulListP :: Parser Block
ulListP =
  UnorderedList <$> do
    wsP $ string "- " <|> string "*" -- first hyphen must have at least one space after
    firstItem <- lineP
    remainingItems <- many $
      do
        wsP $ string "-" <|> string "*"
        lineP
    return $ firstItem : remainingItems

-- parses for an ordered list (1. list item)
olListP :: Parser Block
olListP =
  OrderedList <$> do
    startVal <- int
    wsP (string ". ")
    firstItem <- lineP
    remainingItems <- many $
      do
        int <* wsP (string ".")
        lineP
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
      lineP

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
      manyTill lineP (try (string "```\n"))

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
int = read <$> many1 digit-}