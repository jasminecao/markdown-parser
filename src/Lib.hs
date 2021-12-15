module Lib where

import Data.Char (isSpace)
import Text.Parsec.Token
import Text.ParserCombinators.Parsec

-- | Parses for an integer
int :: Parser Int
int = read <$> many1 digit

-- | Parses for the content between square brackets
bracketsP :: Parser a -> Parser a
bracketsP p = string "[" *> p <* string "]"

-- | Parses for the content between parentheses
parensP :: Parser a -> Parser a
parensP p = string "(" *> p <* string ")"

-- | Parser to consume '\n' character
newLineChar :: Parser String
newLineChar = string "\n"

-- | Parses for text between a beginning and end string
betweenP :: String -> Parser String
betweenP str = between (string str) (string str) $ many1 (noneOf (str ++ "\n"))

-- | Parses for the string between quotes
quotesP :: Parser String
quotesP = between (string "\"") (string "\"") $ many (noneOf "\"")

-- | An adjustment of Parsec's manyTill to not accept the empty string
-- Applies p one or more times until parser end succeeds
many1Till :: Parser a -> Parser b -> Parser [a]
many1Till p end = do
  x <- p
  xs <- manyTill p end
  return (x : xs)

-- | Removes trailing whitespace
wsP :: Parser a -> Parser a
wsP p = p <* many (satisfy isSpace)