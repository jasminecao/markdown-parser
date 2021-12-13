module HTMLParser where

import qualified Control.Monad as Monad
import Data.Char (isSpace)
import Data.Functor (($>))
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

quotesP :: Parser String
quotesP = betweenP "\""

openingTag :: String -> Parser String
openingTag tag = string $ '<' : tag ++ ">"

attr :: String -> Parser String
attr name = string name *> string "=" *> quotesP

openingWithAttr :: String -> String -> Parser String
openingWithAttr tag name = wsP (string ('<' : tag)) *> attr name <* string ">"

closingTag :: String -> Parser String
closingTag tag = string $ '<' : '/' : tag ++ ">"

-- Parser between tags
container :: Parser b -> Parser c -> Parser a -> Parser a
container open close p = try open *> p <* try close

-- TODO check the two are the same
-- container tag p = if x == tag && z == tag then y else Left "no parses" where
--                 (x,y,z) = (,,) <$> openingTag
--                             <*> p
--                             <*> closingTag

simpleContainer :: String -> Parser a -> Parser a
simpleContainer tag = container (openingTag tag) (closingTag tag)

simpleContainerTest :: String -> Parser a -> Parser [a]
simpleContainerTest tag p = try (openingTag tag) *> manyTill p (try (closingTag tag))

parseHtml :: String -> Either ParseError Doc
parseHtml = parse htmlP ""

htmlP :: Parser Doc
htmlP = Doc <$> many1 hBlockP

hBlockP :: Parser Block
hBlockP = tryBlockP <* many (string "\n")
  where
    tryBlockP =
      try hImgP
        <|> try hBrP
        <|> try hHrP
        <|> try hHeadingP
        <|> try hUlListP
        <|> try hOListP
        <|> try hQuoteP
        <|> try hCodeBlockP
        <|> hParagraphP

-- parses headings
-- <h1>HEADING</h1> <a href=\"url\">ONE</a></h1>
hHeadingP :: Parser Block
hHeadingP = choice [checkHeader i | i <- [1 .. 6]]
  where
    checkHeader :: Int -> Parser Block
    checkHeader i = try $ Heading i . S.Line <$> simpleContainerTest ("h" ++ show i) hTextP

hLiP :: Parser S.Line
hLiP = S.Line <$> simpleContainerTest "li" hTextP

-- TODO: figure out how to implement sublists?
-- parses for an unordered list (- list item)
hUlListP :: Parser Block
hUlListP = UnorderedList <$> simpleContainerTest "ul" hLiP

-- parses for an ordered list (1. list item)
hOListP :: Parser Block
hOListP =
  OrderedList <$> do
    startVal <- int
    wsP (string ". ")
    firstItem <- hLineP
    remainingItems <- many $
      do
        int <* wsP (string ".")
        hLineP
    return (startVal, firstItem : remainingItems)

-- parses for a link <a href=\"url\">stuff</a>
hLinkP :: Parser Text
hLinkP = flip Link <$> openingWithAttr "a" "href" <*> many hTextP <* closingTag "a"

-- parses for a <img src=\"url\">
-- TODO empty alt?
hImgP :: Parser Block
hImgP = Image <$> (wsP (string "<img") *> wsP (attr "alt")) <*> attr "src" <* string ">"

-- wsP (string ('<' : tag))
--Image "alt" <$> openingWithAttr "img" "src"

-- parses for a <blockquote><p>abcde</p><p>fghij</p></blockquote>
hQuoteP :: Parser Block
hQuoteP = BlockQuote <$> simpleContainerTest "blockquote" (S.Line <$> simpleContainerTest "p" hTextP)

-- parses for a <p></p>
-- <p>this is fun\n<i>yes</i></p>
hParagraphP :: Parser Block
hParagraphP = Paragraph . S.Line <$> simpleContainerTest "p" hTextP

-- parses for a code block
-- <pre><code>f :: a -> b\n</code></pre>
hCodeBlockP :: Parser Block
hCodeBlockP = CodeBlock <$> simpleContainer "pre" (simpleContainerTest "code" anyChar)

-- parses for a horizontal line <hr> or <hr/>
hHrP :: Parser Block
hHrP = (try (openingTag "hr") <|> string "<hr/>") $> Hr

-- parses for an newline <br> or </br>
hBrP :: Parser Block
hBrP = (try (openingTag "br") <|> string "<br/>") $> Br

-- parses a line of text to handle style (bold, italics, inline code, etc)
hLineP :: Parser S.Line
hLineP = S.Line <$> many1 hTextP

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
hBoldP = Bold <$> simpleContainerTest "b" anyChar

-- parses for an italic string (*text*)
hItalicP :: Parser Text
hItalicP = Italic <$> simpleContainerTest "i" anyChar

-- parses for a strike through string (~~text~~)
hStrikeP :: Parser Text
hStrikeP = Strikethrough <$> simpleContainerTest "del" anyChar

-- parses for an inline code string (`text`)
hInlineCodeP :: Parser Text
hInlineCodeP = InlineCode <$> simpleContainerTest "code" anyChar

-- parses for a normal, undecorated string
hNormalP :: Parser Text
hNormalP =
  Normal
    <$> ( try
            ( many1Till
                anyChar
                ( choice $
                    [try $ lookAhead (openingTag tag) | tag <- htmlTags]
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

-- parser to consume \n character
newLineChar :: Parser String
newLineChar = string "\n"

-- parses for an integer
int :: Parser Int
int = read <$> many1 digit

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