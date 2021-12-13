module HTMLParser where

import qualified Control.Monad as Monad
import Data.Char (isSpace)
import Data.Functor (($>))
import Syntax (Block (..), Doc (Doc), Line, TableBody (..), TableCell (..), TableHead (..), TableRow (..), Text (..))
import qualified Syntax as S
import Text.Parsec.Token
import Text.ParserCombinators.Parsec

-- for testing (again LOL)
p3 :: Parser a -> String -> Either String a
p3 parser str = case parse parser "" str of
  Left err -> Left "No parses"
  Right x -> Right x

quotesP :: Parser String
quotesP = betweenP "\""

openingTag :: String -> Parser String
openingTag tag = string $ '<' : tag ++ ">"

closingTag :: String -> Parser String
closingTag tag = string $ "</" ++ tag ++ ">"

betweenTag :: String -> Parser a -> Parser a
betweenTag tag p = try (openingTag tag) *> p <* try (closingTag tag)

-- parses p until closing tag is found
container :: String -> Parser a -> Parser [a]
container tag p = try (openingTag tag) *> manyTill p (try (closingTag tag))

-- parses a line between `tag`
lineContainer :: String -> Parser S.Line
lineContainer tag = S.Line <$> container tag hTextP

textContainer :: String -> Parser String
textContainer tag = container tag anyChar

parseHtml :: String -> Either ParseError Doc
parseHtml = parse htmlP ""

htmlP :: Parser Doc
htmlP = Doc <$> container "html" hBlockP

hBlockP :: Parser Block
hBlockP =
  try hImgP
    <|> try hBrP
    <|> try hHrP
    <|> try hHeadingP
    <|> try hUlListP
    <|> try hOlListP
    <|> try hQuoteP
    <|> try hCodeBlockP
    <|> hParagraphP

-- parses headings
-- <h1>HEADING <a href=\"url\">ONE</a></h1>
hHeadingP :: Parser Block
hHeadingP = choice [checkHeader i | i <- [1 .. 6]]
  where
    checkHeader :: Int -> Parser Block
    checkHeader i = try $ Heading i <$> lineContainer ("h" ++ show i)

hLiP :: Parser S.Line
hLiP = lineContainer "li"

-- TODO: figure out how to implement sublists?
-- parses for an unordered list (- list item)
hUlListP :: Parser Block
hUlListP = UnorderedList <$> container "ul" hLiP

-- parses for an ordered list (1. list item)
hOlListP :: Parser Block
hOlListP = OrderedList <$> ((,) <$> (read <$> openingWithAttr "ol" "start") <*> manyTill hLiP (try (closingTag "ol")))

attr :: String -> Parser String
attr name = string name *> string "=" *> quotesP

openingWithAttr :: String -> String -> Parser String
openingWithAttr tag name = wsP (string ('<' : tag)) *> wsP (attr name) <* string ">"

-- parses for a link <a href=\"url\">stuff</a>
hLinkP :: Parser Text
hLinkP =
  flip Link
    <$> (try (openingWithAttr "a" "href") <|> openingTag "a" $> "")
    <*> manyTill (try hTextP) (try (closingTag "a"))

-- parses for a <img src=\"url\">
hImgP :: Parser Block
hImgP = Image <$> (wsP (string "<img") *> wsP (attr "alt")) <*> attr "src" <* string ">"

-- parses for a <blockquote><p>abcde</p><p>fghij</p></blockquote>
hQuoteP :: Parser Block
hQuoteP = BlockQuote <$> container "blockquote" (S.Line <$> container "p" hTextP)

-- parses for a <p></p>
hParagraphP :: Parser Block
hParagraphP = Paragraph <$> lineContainer "p"

-- parses for a code block
hCodeBlockP :: Parser Block
hCodeBlockP = CodeBlock <$> betweenTag "pre" (container "code" anyChar)

-- parses for a horizontal line <hr> or <hr/>
hHrP :: Parser Block
hHrP = (try (openingTag "hr") <|> string "<hr/>") $> Hr

-- parses for an newline <br> or </br>
hBrP :: Parser Block
hBrP = (try (openingTag "br") <|> string "<br/>") $> Br

-- parses a line of text to handle style (bold, italics, inline code, etc)
hLineP :: Parser S.Line
hLineP = S.Line <$> many1 hTextP

hTableP :: Parser Block
hTableP = Table <$> hTableHeadP <*> hTableBodyP

hTableHeadP :: Parser TableHead
hTableHeadP = TableHead . TableRow <$> container "tr" hTableCellP

hTableBodyP :: Parser TableBody
hTableBodyP = TableBody <$> container "tbody" hTableRowP

hTableRowP :: Parser TableRow
hTableRowP = TableRow <$> container "tr" hTableCellP

hTableCellP :: Parser TableCell
hTableCellP = TableCell <$> lineContainer "td"

-- parses for a text string
hTextP :: Parser Text
hTextP =
  try hLinkP
    <|> try hItalicP
    <|> try hBoldP
    <|> try hStrikeP
    <|> try hInlineCodeP
    <|> try hNormalP

-- parses for text between a beginning and end string
betweenP :: String -> Parser String
betweenP str = between (string str) (string str) $ many (noneOf (str ++ "\n"))

-- parses for a bold string (**text**)
hBoldP :: Parser Text
hBoldP = Bold <$> textContainer "b"

-- parses for an italic string (*text*)
hItalicP :: Parser Text
hItalicP = Italic <$> textContainer "i"

-- parses for a strike through string (~~text~~)
hStrikeP :: Parser Text
hStrikeP = Strikethrough <$> textContainer "del"

-- parses for an inline code string (`text`)
hInlineCodeP :: Parser Text
hInlineCodeP = InlineCode <$> textContainer "code"

-- parses for a normal, undecorated string
hNormalP :: Parser Text
hNormalP =
  Normal
    <$> ( try
            ( many1Till
                anyChar
                ( choice $
                    [try $ lookAhead (openingTag tag) | tag <- htmlTags]
                      ++ [ try $
                             lookAhead
                               (string ('<' : tag) *> manyTill anyChar (string ">"))
                           | tag <- htmlTags
                         ]
                      ++ [try $ lookAhead (closingTag tag) | tag <- htmlTags]
                )
            )
            <|> many1 anyChar
        )

many1Till :: Parser a -> Parser b -> Parser [a]
many1Till p end = do
  x <- p
  xs <- manyTill p end
  return (x : xs)

-- removes trailing whitespace
wsP :: Parser a -> Parser a
wsP p = p <* many (satisfy isSpace)

-- TODO: move to Syntax.hs?
htmlTags :: [String]
htmlTags =
  [ "html",
    "blockquote",
    "code",
    "pre",
    "img",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "hr",
    "br",
    "ol",
    "ul",
    "li",
    "p",
    "i",
    "b",
    "a",
    "del",
    "table",
    "tbody",
    "thead",
    "tfoot",
    "tbody",
    "td",
    "th",
    "tr"
  ]