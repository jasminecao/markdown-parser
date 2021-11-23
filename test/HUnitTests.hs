module HUnitTests where

import Test.HUnit
import Text.ParserCombinators.Parsec
import Text.Parsec.Token

p :: Parser a -> String -> Either ParseError a
p parser = parse parser ""

test_block =
  "parsing block"
    ~: TestList
      []