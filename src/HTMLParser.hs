module HTMLParser where

import qualified Control.Monad as Monad
import Data.Char (isSpace)
import Data.Functor (($>))
import Lib
import Syntax (Block (..), Doc (Doc), Line, TableBody (..), TableCell (..), TableHead (..), TableRow (..), Text (..), htmlTags)
import qualified Syntax as S
import Text.Parsec.Token
import Text.ParserCombinators.Parsec

{- HTML parsers -}

-- | Parses the complete file or text into a Doc type
parseHtml :: String -> Either ParseError Doc
parseHtml = parse htmlP ""

-- | Parses a Doc for multiple blocks
htmlP :: Parser Doc
htmlP = Doc <$> container "html" hBlockP

{- Block parsers -}

-- | Parses for a block of HTML (headings, lists, quotes, code blocks,...)
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
    <|> try hTableP
    <|> hParagraphP

-- | Parses headings (<h1>HEADING <a href=\"url\">ONE</a></h1>)
hHeadingP :: Parser Block
hHeadingP = choice [checkHeader i | i <- [1 .. 6]]
  where
    -- Creates a parser for the a heading container, given a number
    checkHeader :: Int -> Parser Block
    checkHeader i = try $ Heading i <$> lineContainer ("h" ++ show i)

-- | Parses for a list item
hLiP :: Parser S.Line
hLiP = lineContainer "li"

-- | Parses for an unordered list (<ul><li></li>...</ul>)
hUlListP :: Parser Block
hUlListP = UnorderedList <$> container "ul" hLiP

-- | Parses for an ordered list (<ol><li></li>...</ol>)
hOlListP :: Parser Block
hOlListP = OrderedList <$> ((,) <$> (read <$> openingWithAttr "ol" "start") <*> manyTill hLiP (try (closingTag "ol")))

-- | Parses for an image (<img src=\"url\">()
hImgP :: Parser Block
hImgP = Image <$> (wsP (string "<img") *> wsP (attr "alt")) <*> attr "src" <* string ">"

-- | Parses for a blockquote (<blockquote><p>...</p>...</blockquote>)
hQuoteP :: Parser Block
hQuoteP = BlockQuote <$> container "blockquote" (S.Line <$> container "p" hTextP)

-- | Parses for a paragraph (<p>...</p>)
hParagraphP :: Parser Block
hParagraphP = Paragraph <$> lineContainer "p"

-- | Parses for a code block
hCodeBlockP :: Parser Block
hCodeBlockP = CodeBlock <$> betweenTag "pre" (container "code" anyChar)

-- | Parses for a horizontal line <hr> or <hr/>
hHrP :: Parser Block
hHrP = (try (openingTag "hr") <|> string "<hr/>") $> Hr

-- | Parses for an newline <br> or </br>
hBrP :: Parser Block
hBrP = (try (openingTag "br") <|> string "<br/>") $> Br

-- | Parses a line of text to handle style (bold, italics, inline code, etc)
hLineP :: Parser S.Line
hLineP = S.Line <$> many1 hTextP

-- | Parses for a table through a table head and table body
hTableP :: Parser Block
hTableP =
  Table
    <$> (openingTag "table" *> hTableHeadP)
    <*> (hTableBodyP <* closingTag "table")

{- Table parsers -}

-- | Parses for a thead (<thead><tr>...</tr></thead>) with one row
hTableHeadP :: Parser TableHead
hTableHeadP = TableHead <$> betweenTag "thead" (hTableRowP "th")

-- | Parses for a tbody (<tbody><tr>...</tr>...</tbody>)
hTableBodyP :: Parser TableBody
hTableBodyP = TableBody <$> container "tbody" (hTableRowP "td")

-- | Parses for a tr (<tr>...</tr>) with specifications for which kind of table cell (td or th)
hTableRowP :: String -> Parser TableRow
hTableRowP cellTag = TableRow <$> container "tr" (hTableCellP cellTag)

-- | Parses for a td or th (<td>...</td>, <th>...</th>)
hTableCellP :: String -> Parser TableCell
hTableCellP cellTag = TableCell <$> lineContainer cellTag

{- Text parsers -}

-- | Parses for a text string
hTextP :: Parser Text
hTextP =
  try hLinkP
    <|> try hItalicP
    <|> try hBoldP
    <|> try hStrikeP
    <|> try hInlineCodeP
    <|> try hNormalP

-- | Parses for a link <a href=\"url\">stuff</a>
hLinkP :: Parser Text
hLinkP =
  flip Link
    <$> (try (openingWithAttr "a" "href") <|> openingTag "a" $> "")
    <*> manyTill (try hTextP) (try (closingTag "a"))

-- | Parses for a bold string (<b>text</b>)
hBoldP :: Parser Text
hBoldP = Bold <$> textContainer "b"

-- | Parses for an italic string (<i>text</i>)
hItalicP :: Parser Text
hItalicP = Italic <$> textContainer "i"

-- | Parses for a strike through string (<del>text</del>)
hStrikeP :: Parser Text
hStrikeP = Strikethrough <$> textContainer "del"

-- | Parses for an inline code string (<code>text</code>)
hInlineCodeP :: Parser Text
hInlineCodeP = InlineCode <$> textContainer "code"

-- | Parses for a normal, undecorated string
hNormalP :: Parser Text
hNormalP =
  Normal
    <$> ( try
            ( many1Till -- parses until we reach
                anyChar
                ( choice $
                    [try $ lookAhead (openingTag tag) | tag <- htmlTags] -- an opening tag <tag>
                      ++ [ try $
                             lookAhead
                               (string ('<' : tag) *> manyTill anyChar (string ">"))
                           | tag <- htmlTags
                         ] -- an opening tag with attributes <tag attr="something">
                      ++ [try $ lookAhead (closingTag tag) | tag <- htmlTags]
                      -- a closing tag </tag>
                )
            )
            <|> many1 anyChar -- or parse anything until we can't anymore
        )

{- Helper functions -}

-- | Parses for an HTML opening tag <tag>
openingTag :: String -> Parser String
openingTag tag = string $ '<' : tag ++ ">"

-- | Parses for an HTML closing tag </tag>
closingTag :: String -> Parser String
closingTag tag = string $ "</" ++ tag ++ ">"

-- | Parses for the content between HTML opening and closing tags <tag>...</tag>
betweenTag :: String -> Parser a -> Parser a
betweenTag tag p = try (openingTag tag) *> p <* try (closingTag tag)

-- | Parses p until closing tag is found
container :: String -> Parser a -> Parser [a]
container tag p = try (openingTag tag) *> manyTill p (try (closingTag tag))

-- | Parses a line between `tag`
lineContainer :: String -> Parser S.Line
lineContainer tag = S.Line <$> container tag hTextP

-- | Parses the string between `tag`
textContainer :: String -> Parser String
textContainer tag = container tag anyChar

-- | Parses for the value of an attribute (attr="value")
attr :: String -> Parser String
attr name = string name *> string "=" *> quotesP

-- | Parses the value of an attribute from an opening tag (<tag attr="value">)
openingWithAttr :: String -> String -> Parser String
openingWithAttr tag name = wsP (string ('<' : tag)) *> wsP (attr name) <* string ">"