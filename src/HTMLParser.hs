module HTMLParser where

import qualified Control.Monad as Monad
import Data.Char (isSpace)
import Syntax (Block (..), Doc (Doc), Line, Text (..))
import Data.Functor (($>))
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

quotesP :: Parser String
quotesP = betweenP "\""

openingTag :: String -> Parser String
openingTag tag = string $ '<' : tag ++ ">"

attr :: String -> Parser String 
attr name = string name *> string "=" *> quotesP

openingWithAttr :: String -> String -> Parser String
openingWithAttr tag name = (wsP $ string ('<' : tag)) *> attr name <* string ">"

-- openingTagWithAttrs :: String -> Parser String
-- openingTagWithAttrs tag = string $ '<' : tag ++ ">"

closingTag :: String -> Parser String
closingTag tag = string $ '<' : '/' : tag ++ ">"

-- Parser between tags
container :: Parser b -> Parser c -> Parser a -> Parser a
container open close p = open *> p <* close
-- TODO check the two are the same
-- container tag p = if x == tag && z == tag then y else Left "no parses" where
--                 (x,y,z) = (,,) <$> openingTag
--                             <*> p
--                             <*> closingTag

simpleContainer :: String -> Parser a -> Parser a
simpleContainer tag = container (openingTag tag) (closingTag tag)

parseHtml :: String -> Either ParseError Doc
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

hLiP :: Parser String 
hLiP = simpleContainer "li" text

-- TODO: figure out how to implement sublists?
-- parses for an unordered list (- list item)
hUListP :: Parser Block
hUListP =
  UnorderedList <$> do
    wsP $ string "- " <|> string "*" -- first hyphen must have at least one space after
    firstItem <- lineP
    remainingItems <- many $
      do
        wsP $ string "-" <|> string "*"
        lineP
    return $ firstItem : remainingItems

-- parses for an ordered list (1. list item)
hOListP :: Parser Block
hOListP =
  OrderedList <$> do
    startVal <- int
    wsP (string ". ")
    firstItem <- lineP
    remainingItems <- many $
      do
        int <* wsP (string ".")
        lineP
    return (startVal, firstItem : remainingItems)

-- parses for a link <a href=\"url\">stuff<\a>
hLinkP :: Parser Text
hLinkP =  flip Link <$> openingWithAttr "a" "href" <*> many textP <* closingTag "a"

-- parses for a <img src=\"url\">
-- TODO empty alt?
hImgP :: Parser Block
hImgP = Image "alt" <$> openingWithAttr "img" "src"

-- parses for a quote block (> quote)
hQuoteP :: Parser Block
hQuoteP = BlockQuote <$> many1 quoteNewLinesP
  where
    quoteNewLinesP :: Parser S.Line
    quoteNewLinesP = do
      wsP $ char '>'
      lineP

-- parses for a paragraph
hParagraphP :: Parser Block
hParagraphP = Paragraph <$> lineP

-- parses for a code block (```\n code \n```)
hCodeBlockP :: Parser Block
hCodeBlockP = CodeBlock <$> codeNewLinesP
  where
    codeNewLinesP :: Parser [S.Line]
    codeNewLinesP = do
      string "```\n"
      manyTill lineP (try (string "```\n"))

-- parses for a horizontal line <hr> or <hr/>
hHrP :: Parser Block
hHrP = (openingTag "hr" <|> string "<hr/>") $> Hr

-- parses for an newline <br> or </br>
hBrP :: Parser Block
hBrP = (openingTag "br" <|> string "<br/>") $> Br

-- parses a line of text to handle style (bold, italics, inline code, etc)
lineP :: Parser S.Line
lineP = S.Line <$> many1 textP <* char '\n'

-- parses for a text string
textP :: Parser Text
textP =
  try hLinkP
    <|> try hItalicP
    <|> try hBoldP
    <|> try hStrikeP
    <|> try hInlineCodeP
    <|> try hNormalP

-- parses for text between a beginning and end string
betweenP :: String -> Parser String
betweenP str = between (string str) (string str) $ many1 (noneOf (str ++ "\n"))

-- parses for a bold string (**text**)
hBoldP :: Parser Text
hBoldP = Bold <$> simpleContainer "b" text

-- parses for an italic string (*text*)
hItalicP :: Parser Text
hItalicP = Italic <$> simpleContainer "i" text

-- parses for a strike through string (~~text~~)
hStrikeP :: Parser Text
hStrikeP = Strikethrough <$> (simpleContainer "strike" text <|> simpleContainer "s" text)

-- parses for an inline code string (`text`)
hInlineCodeP :: Parser Text
hInlineCodeP = InlineCode <$> simpleContainer "code" text

-- parses for a normal, undecorated string
hNormalP :: Parser Text
hNormalP = Normal <$> text

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