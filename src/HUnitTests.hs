module HUnitTests where

import Data.Char (isSpace)
import MarkdownParser
import SampleText
import Syntax (Block (..), Doc (Doc), Line (..), Text (..), reservedMarkdownChars)
import qualified Syntax as S
import Test.HUnit (Test (TestList), runTestTT, (~:), (~?=))
import Text.Parsec.Token
import Text.ParserCombinators.Parsec

p :: Parser a -> String -> Either String a
p parser str = case parse parser "" str of
  Left err -> Left "No parses"
  Right x -> Right x

test_boldP =
  "bold"
    ~: TestList
      [ p boldP "**bold word**" ~?= Right (Bold "bold word"),
        p boldP "*not bold*" ~?= Left "No parses",
        p boldP "**also not bold" ~?= Left "No parses"
      ]

test_italicP =
  "italic"
    ~: TestList
      [ p italicP "*italic word*" ~?= Right (Italic "italic word"),
        p italicP "**not bold**" ~?= Left "No parses",
        p italicP "*also not italic" ~?= Left "No parses"
      ]

test_strikeP =
  "strikethrough"
    ~: TestList
      [ p strikeP "~~strikethrough~~" ~?= Right (Strikethrough "strikethrough"),
        p strikeP "~not strike**" ~?= Left "No parses",
        p strikeP "also not strike" ~?= Left "No parses",
        p strikeP "~~also not \n strikethrough" ~?= Left "No parses"
      ]

test_inlineCodeP =
  "inline code"
    ~: TestList
      [ p inlineCodeP "`regular code`" ~?= Right (InlineCode "regular code"),
        p inlineCodeP "```code```" ~?= Right (InlineCode "code"),
        p inlineCodeP "`not code" ~?= Left "No parses"
      ]

test_normalP =
  "normal text"
    ~: TestList
      [ p normalP "regular text" ~?= Right (Normal "regular text"),
        p normalP "`not text`" ~?= Left "No parses"
      ]

test_textP =
  "text"
    ~: TestList
      [ p textP "regular text" ~?= Right (Normal "regular text"),
        p textP "`code`" ~?= Right (InlineCode "code"),
        p textP "~~strikethrough~~" ~?= Right (Strikethrough "strikethrough"),
        p textP "**bold**" ~?= Right (Bold "bold"),
        p textP "*italic*" ~?= Right (Italic "italic"),
        p textP "`code test this" ~?= Right (Normal "`code test this")
      ]

test_lineP =
  "line"
    ~: TestList []

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
        p ulListP "- item 1\ndontparsethis" ~?= Right (UnorderedList [S.Line [Normal "item 1"]]),
        p ulListP "- item 1\n- item 2\n" ~?= Right (UnorderedList [S.Line [Normal "item 1"], S.Line [Normal "item 2"]]),
        p ulListP "- item 1\n-item 2\n" ~?= Right (UnorderedList [S.Line [Normal "item 1"], S.Line [Normal "item 2"]])
      ]

test_olListP =
  "ordered list"
    ~: TestList
      [ p olListP "1.1" ~?= Left "No parses",
        p olListP "1. item 1\n" ~?= Right (OrderedList [S.Line [Normal "item 1"]]),
        p olListP "1. item 1\n2. item 2\n" ~?= Right (OrderedList [S.Line [Normal "item 1"], S.Line [Normal "item 2"]]),
        p olListP "11. item 1\n2. item 2\n" ~?= Right (OrderedList [S.Line [Normal "item 1"], S.Line [Normal "item 2"]]),
        -- TODO: figure out why this doesn't pass (but it does with a space)
        p olListP "1. item 1\n2.item 2\n" ~?= Right (OrderedList [S.Line [Normal "item 1\n2.item 2"]])
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
        p quoteP ">hello\n" ~?= Right (BlockQuote [S.Line [Normal "hello"]]),
        p quoteP ">1\n>2\n" ~?= Right (BlockQuote [S.Line [Normal "1"], S.Line [Normal "2"]]),
        p quoteP ">1\n>2\n> 3\n" ~?= Right (BlockQuote [S.Line [Normal "1"], S.Line [Normal "2"], S.Line [Normal "3"]])
      ]

test_paragraph =
  "paragraph"
    ~: TestList
      [ p paragraphP "regular string\n"
          ~?= Right
            (Paragraph (S.Line [Normal "regular string"])),
        p paragraphP "regular string `code` and *italics*\n"
          ~?= Right (Paragraph (S.Line [Normal "regular string ", InlineCode "code", Normal " and ", Italic "italics"]))
      ]

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
      [ test_boldP,
        test_italicP,
        test_strikeP,
        test_inlineCodeP,
        test_normalP,
        test_textP,
        test_lineP,
        test_headingP,
        test_codeBlockP,
        test_paragraph,
        test_ulListP,
        test_olListP,
        test_blockQuote,
        test_brPHrP,
        test_table,
        test_block
      ]