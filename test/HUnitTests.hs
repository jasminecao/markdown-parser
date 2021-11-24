module HUnitTests where

import Test.HUnit
import MarkdownParser
import SampleText
import Data.Char (isSpace)
import Syntax (Block (..), Doc (Doc), Line (..), Text (..), reservedMarkdownChars)
import qualified Syntax as S
import Text.Parsec.Token
import Text.ParserCombinators.Parsec

p :: Parser a -> String -> Either String a
p parser str = case parse parser "" str of
  Left err -> Left "No parses"
  Right x -> Right x

test_headingP =
  "heading"
    ~: TestList
      [ p headingP "# Heading 1" ~?= Right (Heading 1 (S.Line [Normal "Heading 1"])),
        p headingP "#### Heading 4" ~?= Right (Heading 4 (S.Line [Normal "Heading 4"])),
        p headingP "####### Heading 7" ~?= Left "No parses",
        p headingP "Heading 1" ~?= Left "No parses",
        p headingP "## # Heading 2" ~?= Right (Heading 2 (S.Line [Normal "# Heading 2"]))
      ]

test_ulListP =
  "unordered list"
    ~: TestList
      [ p ulListP "-1" ~?= Left "No parses",
        p ulListP "- item 1" ~?= Right (UnorderedList [S.Line [Normal "item 1"]]),
        p ulListP "- item 1\n- item 2" ~?= Right (UnorderedList [S.Line [Normal "item 1"], S.Line [Normal "item 2"]]),
        p ulListP "- item 1\n-item 2" ~?= Right (UnorderedList [S.Line [Normal "item 1\n-item 2"]])
      ]

test_olListP =
  "ordered list"
    ~: TestList
      [ p olListP "1.1" ~?= Left "No parses",
        p olListP "1. item 1" ~?= Right (OrderedList [S.Line [Normal "item 1"]]),
        p olListP "1. item 1\n2. item 2" ~?= Right (OrderedList [S.Line [Normal "item 1"], S.Line [Normal "item 2"]]),
        p olListP "1. item 1\n2.item 2" ~?= Right (OrderedList [S.Line [Normal "item 1\n2.item 2"]])
      ]

test_linkP =
  "link"
    ~: TestList
      [ p linkP "[()]" ~?= Left "No parses",
        p linkP "[google](google.com" ~?= Left "No parses",
        p linkP "[google]\n(google.com)" ~?= Left "No parses",
        p linkP "[google](google.com)" ~?= Right (Link "google.com" (Line [Normal "google"])),
        p linkP "[](google.com)" ~?= Right (Link "google.com" (Line [Normal ""])),
        p linkP "[google]()" ~?= Right (Link "" (Line [Normal "google.com"]))
      ]

test_blockQuote =
  "block quote"
    ~: TestList
      [ p quoteP "``` ``" ~?= Left "No parses",
        p quoteP ">hello" ~?= Right (BlockQuote "hello"),
        p quoteP ">1\n>2" ~?= Right (BlockQuote "1\n2"),
        p quoteP ">1\n>2\n> 3" ~?= Right (BlockQuote "1\n2\n 3")
      ]

test_codeBlockP =
  "code block"
    ~: TestList
      [ p codeBlockP "``` ``" ~?= Left "No parses",
        p codeBlockP "``````" ~?= Left "No parses",
        p codeBlockP "```hello!```" ~?= Right (CodeBlock "hello!"),
        p codeBlockP "```a line\nanother line```" ~?= Right (CodeBlock "a line\nanother line")
      ]

test_brPHrP =
  "br and hr"
    ~: TestList
      [ p hrP "--" ~?= Left "No parses",
        p hrP "---" ~?= Right Hr,
        p hrP "--------" ~?= Right Hr
      ]

test_table =
  "table"
    ~: TestList
      [ 
      ]

test_block =
  "parsing block"
    ~: TestList
      []

test_all = runTestTT $ TestList [test_headingP, test_codeBlockP, test_ulListP, test_olListP, test_blockQuote]