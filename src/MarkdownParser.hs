module MarkdownParser where

import qualified Control.Monad as Monad
import Data.Char (isSpace)
import Data.Functor (($>))
import Syntax (Block (..), Doc (Doc), Line, TableBody, TableCell, TableHead, TableRow, Text (..))
import qualified Syntax as S
import Text.Parsec.Token
import Text.ParserCombinators.Parsec as Parsec

-- TODO: delete this. for testing......
p2 :: Parser a -> String -> Either String a
p2 parser str = case parse parser "" str of
  Left err -> Left "No parses"
  Right x -> Right x

{- Markdown parsers -}

-- | Parses the complete file or text into a Doc type.
-- TODO: add error filepath arg to parse
parseMarkdown :: String -> Either ParseError Doc
parseMarkdown = parse markdownP ""

-- | Parses a Doc from many blocks
markdownP :: Parser Doc
markdownP = Doc <$> many1 blockP


{- Block parsers -}

-- | Parses for a block of markdown (headings, lists, quotes, code blocks)
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
        <|> try tableP
        <|> paragraphP

-- | Parses for # heading and converts rest of line to Line
headingP :: Parser Block
headingP = do
  hx <- wsP $ many1 (char '#')
  Monad.guard (length hx < 7)
  Heading (length hx) <$> lineP

-- TODO: figure out how to implement sublists?
-- | Parses for an unordered list (- list item)
ulListP :: Parser Block
ulListP =
  UnorderedList <$> do
    wsP (string "- " <|> string "* ") -- first hyphen must have at least one space after
    firstItem <- lineP
    remainingItems <- many $
      do
        wsP (string "-" <|> string "*")
        lineP
    return $ firstItem : remainingItems

-- | Parses for an ordered list (1. list item)
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

-- | Parses for a table
tableP :: Parser Block
tableP = do
  firstRow <- rowP <* try theadSeparatorP
  remainingRows <- many rowP
  return $ Table (S.TableHead firstRow) (S.TableBody remainingRows)
  where
    -- parses the pipe character and any spaces/tabs following
    pipeP :: Parser String
    pipeP = string "|" <* many (string " " <|> string "\t")

    -- parses for a row of header separators | --- | --- | --- |
    theadSeparatorP :: Parser ()
    theadSeparatorP =
      ( pipeP
          *> manyTill
            ( do
                hx <- manyTill (char '-') (many (satisfy isSpace) *> pipeP)
                -- needs to be at least three --- to parse as a header
                Monad.guard (length hx >= 3)
            )
            newLineChar
      )
        $> ()

    -- parses a row of table cells
    rowP :: Parser TableRow
    rowP =
      pipeP
        *> ( S.TableRow
               <$> manyTill (S.TableCell . S.Line <$> manyTill (try textP) (try pipeP)) newLineChar
           )

-- | Parses for an image (![alt](src "title"))
imgP :: Parser Block
imgP = string "!" *> (Image <$> bracketsP (many (noneOf "]")) <*> parensP (many1 (noneOf ")")))

-- | Parses for a quote block (> quote)
quoteP :: Parser Block
quoteP = BlockQuote <$> many1 quoteNewLinesP
  where
    quoteNewLinesP :: Parser S.Line
    quoteNewLinesP = do
      wsP $ char '>'
      lineP

-- | Parses for a paragraph
paragraphP :: Parser Block
paragraphP = Paragraph <$> lineP

-- | Parses for a code block (```\n code \n```)
codeBlockP :: Parser Block
codeBlockP = CodeBlock <$> (string "```\n" *> manyTill anyChar (try (string "```\n")))

-- | Parses for a horizontal link (---)
hrP :: Parser Block
hrP = string "---" $> Hr

-- | Parses for an empty line
brP :: Parser Block
brP = string "\n\n" $> Br


{- Line parser -}

-- | Parses a line of text to handle style (bold, italics, inline code, etc)
lineP :: Parser S.Line
lineP = S.Line <$> many1 textP <* char '\n'


{- Text parsers -}

-- | Parses for a text string
textP :: Parser Text
textP =
  try linkP
    <|> try italicP
    <|> try boldP
    <|> try strikeP
    <|> try inlineCodeP
    <|> try normalP

-- | Parses for a link ([text](link))
linkP :: Parser Text
linkP = S.Link <$> bracketsP (manyTill textP (string "]")) <*> parensP (many (noneOf ")"))

-- | Parses for a bold string (**text**)
boldP :: Parser Text
boldP = Bold <$> (betweenP "**" <|> betweenP "__")

-- | Parses for an italic string (*text*)
italicP :: Parser Text
italicP = Italic <$> (betweenP "*" <|> betweenP "_")

-- | Parses for a strike through string (~~text~~)
strikeP :: Parser Text
strikeP = Strikethrough <$> betweenP "~~"

-- | Parses for an inline code string (`text`)
inlineCodeP :: Parser Text
inlineCodeP = InlineCode <$> betweenP "`"

-- | Parses for a normal, undecorated string
normalP :: Parser Text
normalP =
  try (Normal <$> stringP)
    <|> Normal <$> many1 (noneOf "\n")


{- Helper functions -}

-- | Parses for the string between a beginning and end string
betweenP :: String -> Parser String
betweenP str = between (string str) (string str) $ many1 (noneOf (str ++ "\n"))

-- | Parses for the content between square brackets
bracketsP :: Parser a -> Parser a
bracketsP p = string "[" *> p <* optional (string "]")

-- | Parses for the content between parentheses
parensP :: Parser a -> Parser a
parensP p = string "(" *> p <* string ")"

-- TODO: add this to syntax?
-- | Parses for a string until a reserved character is found
stringP :: Parser String
stringP = many1 $ noneOf ['*', '~', '`', '>', '_', '[', ']', '|', '\n']

-- | Removes trailing whitespace
wsP :: Parser a -> Parser a
wsP p = p <* many (satisfy isSpace)

-- | Parser to consume '\n' character
newLineChar :: Parser String
newLineChar = string "\n"

-- | Parses for an integer
int :: Parser Int
int = read <$> many1 digit