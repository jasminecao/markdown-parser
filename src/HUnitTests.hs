module HUnitTests where

import MarkdownParser
import SampleText
import Syntax (Content (..), EType (..), reservedMarkdownChars)
import Test.HUnit
import Text.Parsec.Token
import Text.ParserCombinators.Parsec

p :: Parser a -> String -> Either String a
p parser str = case parse parser "" str of
  Left err -> Left "No parses"
  Right x -> Right x

test_headingP =
  "heading"
    ~: TestList
      [ p headingP "# Heading 1" ~?= Right (E (Heading 1) (Text "Heading 1")),
        p headingP "#### Heading 4" ~?= Right (E (Heading 4) (Text "Heading 4")),
        p headingP "####### Heading 7" ~?= Left "No parses",
        p headingP "Heading 1" ~?= Left "No parses",
        p headingP "## # Heading 2" ~?= Right (E (Heading 2) (Text "# Heading 2"))
      ]

test_ulListP =
  "unordered list"
    ~: TestList
      [ p ulListP "- item 1" ~?= Right (E (UnorderedList [E LItem (Text "item 1")]) Empty)
      ]

test_codeBlockP =
  "code block"
    ~: TestList
      [ p codeBlockP "``````" ~?= Right (E CodeBlock (Text "")),
        p codeBlockP "```hello!```" ~?= Right (E CodeBlock (Text "hello!")),
        p codeBlockP "```a line\nanother line```" ~?= Right (E CodeBlock (Text "a line\nanother line"))
      ]

test_block =
  "parsing block"
    ~: TestList
      []