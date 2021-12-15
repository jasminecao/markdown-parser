module MarkdownParser where

import qualified Control.Monad as Monad
import Data.Char (isSpace)
import Data.Functor (($>))
import Lib
import Syntax (Block (..), Doc (Doc), Line, TableBody, TableCell, TableHead, TableRow, Text (..))
import qualified Syntax as S
import Text.Parsec.Token
import Text.ParserCombinators.Parsec as Parsec

{- Markdown parsers -}

-- | Parses the complete file or text into a Doc type
parseMarkdown :: String -> Either ParseError Doc
parseMarkdown = parse markdownP ""

-- | Parses a Doc for multiple blocks
markdownP :: Parser Doc
markdownP = Doc <$> many1 blockP

{- Block parsers -}

-- | Parses for a block of markdown (headings, lists, quotes, code blocks,...)
blockP :: Parser Block
blockP = tryBlockP <* many newLineChar
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

-- | Parses for # heading and converts rest of line to a Line.
headingP :: Parser Block
headingP = do
  hx <- wsP $ many1 (char '#')
  Monad.guard (length hx < 7)
  Heading (length hx) <$> lineP

liP :: Parser Block
liP = tryLiP <* many newLineChar
  where
    tryLiP =
      try brP
        <|> try hrP
        <|> try headingP
        <|> try quoteP
        <|> try codeBlockP
        <|> try tableP
        <|> paragraphP

subListP :: Int -> Parser Block
subListP level =
  try (wsP (string $ replicate level '\t') *> subUlListP level)
    <|> (wsP (string $ replicate level '\t') *> subOlListP level)

-- | Parses for an unordered list (- list item)
ulListP :: Parser Block
ulListP = subUlListP 0

subUlListP :: Int -> Parser Block
subUlListP level =
  flip UnorderedList level <$> do
    wsP (string "- " <|> string "* ") -- first hyphen must have at least one space after
    firstItem <- liP
    remainingItems <-
      many
        ( try (subListP (level + 1))
            <|> do
              string (replicate level '\t') *> wsP (string "-" <|> string "*")
              liP
        )
    return $ firstItem : remainingItems

-- | Parses for an ordered list (1. list item)
olListP :: Parser Block
olListP = subOlListP 0

subOlListP :: Int -> Parser Block
subOlListP level =
  flip OrderedList level <$> do
    startVal <- int
    wsP (string ". ")
    firstItem <- liP
    remainingItems <-
      many
        ( try (subListP (level + 1))
            <|> do
              string (replicate level '\t') *> int <* wsP (string ".")
              blockP
        )
    return (startVal, firstItem : remainingItems)

-- | Parses for a table
tableP :: Parser Block
tableP = do
  firstRow <- rowP <* theadSeparatorP
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
               <$> manyTill (S.TableCell . S.Line <$> manyTill (try (specialTextP ['\n', '|'])) (try pipeP)) newLineChar
           )

-- | Parses for an image (![alt](./image.jpg))
imgP :: Parser Block
imgP = string "!" *> (Image <$> bracketsP (many (noneOf "]")) <*> parensP (many1 (noneOf ")")))

-- | Parses for a quote block (> Quote)
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
lineP = S.Line <$> many1 textP <* newLineChar

{- Text parsers -}

-- | Parses for any decorated or normal text
textP :: Parser Text
textP = specialTextP ['\n']

-- | Parses for any decorated or normal text with the exception of specialChars
specialTextP :: [Char] -> Parser Text
specialTextP specialChars = decoratedTextP <|> try (normalP specialChars)

-- | Parses for any decorated text
decoratedTextP :: Parser Text
decoratedTextP =
  try linkP
    <|> try italicP
    <|> try boldP
    <|> try strikeP
    <|> try inlineCodeP

-- | Parses for a link ([text](link))
linkP :: Parser Text
linkP = S.Link <$> (string "[" *> manyTill (specialTextP ['\n', ']']) (string "]")) <*> parensP (many (noneOf ")"))

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

-- | Parses for a normal, undecorated string with exception to specialChars
normalP :: [Char] -> Parser Text
normalP specialChars = Normal <$> (try (manyTill specialCharP (try (lookAhead decoratedTextP))) <|> many1 specialCharP)
  where
    specialCharP :: Parser Char
    specialCharP = noneOf specialChars
