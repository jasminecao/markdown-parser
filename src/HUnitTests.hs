module HUnitTests where

import Data.Char (isSpace)
import MarkdownParser
-- import SampleText
import Syntax (Block (..), Doc (Doc), Line (..), Text (..), reservedMarkdownChars)
import qualified Syntax as S
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
      [ p headingP "# Heading 1\n" ~?= Right (Heading 1 (S.Line [Normal "Heading 1"])),
        p headingP "#### Heading 4\n" ~?= Right (Heading 4 (S.Line [Normal "Heading 4"])),
        p headingP "####### Heading 7\n" ~?= Left "No parses",
        p headingP "Heading 1\n" ~?= Left "No parses",
        p headingP "## # Heading 2\n" ~?= Right (Heading 2 (S.Line [Normal "# Heading 2"]))
      ]

test_ulListP =
  "unordered list"
    ~: TestList
      [ p ulListP "-1\n" ~?= Left "No parses",
        p ulListP "- item 1\n" ~?= Right (UnorderedList [S.Line [Normal "item 1"]]),
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
        p quoteP ">hello" ~?= Right (BlockQuote [S.Line [Normal "hello"]]),
        p quoteP ">1\n>2" ~?= Right (BlockQuote [S.Line [Normal "1"], S.Line [Normal "2"]]),
        p quoteP ">1\n>2\n> 3" ~?= Right (BlockQuote [S.Line [Normal "1"], S.Line [Normal "2"], S.Line [Normal "3"]])
      ]

-- TODO: test for inline code block (```code```)
test_codeBlockP =
  "code block"
    ~: TestList
      [ p codeBlockP "``` ``\n" ~?= Left "No parses",
        p codeBlockP "``````\n" ~?= Left "No parses",
        p codeBlockP "```\n```\n" ~?= Right (CodeBlock []),
        p codeBlockP "```\nhello!\n```\n" ~?= Right (CodeBlock [S.Line [Normal "hello!"]]),
        p codeBlockP "```\na line\nanother line\n```\n"
          ~?= Right
            ( CodeBlock
                [ S.Line [Normal "a line"],
                  S.Line [Normal "another line"]
                ]
            )
      ]

test_brPHrP =
  "br and hr"
    ~: TestList
      [ p hrP "--" ~?= Left "No parses",
        p hrP "---" ~?= Right Hr,
        p hrP "--------" ~?= Right Hr,
        p brP "\n" ~?= Right Br,
        p brP "" ~?= Left "No parses",
        p brP " " ~?= Left "No parses"
      ]

test_table =
  "table"
    ~: TestList
      []

test_block =
  "parsing block"
    ~: TestList
      [ p blockP "# Heading 1 `code`\n `code`"
          ~?= Right (Heading 1 (S.Line [Normal "Heading 1 ", InlineCode "code"]))
      ]

test_all =
  runTestTT $
    TestList
      [ test_headingP,
        test_codeBlockP,
        test_ulListP,
        test_olListP,
        test_blockQuote,
        test_brPHrP,
        test_table,
        test_block
      ]