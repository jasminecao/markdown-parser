module HUnitTests where

import Data.Char (isSpace)
import HTMLParser
import MarkdownParser
import SampleText
import Syntax (Block (..), Doc (Doc), Line (..), Text (..))
import qualified Syntax as S
import Test.HUnit (Test (TestList), runTestTT, (~:), (~?=))
import Text.Parsec.Token
import Text.ParserCombinators.Parsec

p :: Parser a -> String -> Either String a
p parser str = case parse parser "" str of
  Left err -> Left "No parses"
  Right x -> Right x

{- MARKDOWN TESTS -}

test_boldP =
  "bold"
    ~: TestList
      [ p boldP "**bold word**" ~?= Right (Bold "bold word"),
        p boldP "*not bold*" ~?= Left "No parses",
        p boldP "**also not bold" ~?= Left "No parses",
        p boldP "__bold word__" ~?= Right (Bold "bold word"),
        p boldP "_not bold_" ~?= Left "No parses"
      ]

test_italicP =
  "italic"
    ~: TestList
      [ p italicP "*italic word*" ~?= Right (Italic "italic word"),
        p italicP "**not bold**" ~?= Left "No parses",
        p italicP "*also not italic" ~?= Left "No parses",
        p italicP "_italic word_" ~?= Right (Italic "italic word"),
        p italicP "___not italic_" ~?= Left "No parses"
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
        p inlineCodeP "`special_char`" ~?= Right (InlineCode "special_char"),
        p inlineCodeP "`not code" ~?= Left "No parses"
      ]

test_normalP =
  "normal text"
    ~: TestList
      [ p normalP "regular text" ~?= Right (Normal "regular text")
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
    ~: TestList
      [ p lineP "line\n" ~?= Right (S.Line [Normal "line"]),
        p lineP "`code` line\n" ~?= Right (S.Line [InlineCode "code", Normal " line"]),
        p lineP "~~strike~~ `code` **bold** *italic* line\n"
          ~?= Right (S.Line [Strikethrough "strike", Normal " ", InlineCode "code", Normal " ", Bold "bold", Normal " ", Italic "italic", Normal " line"]),
        -- TODO: perhaps change this so it doesn't output 2 normals?
        p lineP "~~strike~~ `code` *line\n"
          ~?= Right (S.Line [Strikethrough "strike", Normal " ", InlineCode "code", Normal " ", Normal "*line"]),
        p lineP "`code` **line\nend"
          ~?= Right (S.Line [InlineCode "code", Normal " ", Normal "**line"]),
        p lineP "regular text _not**line\n"
          ~?= Right (Line [Normal "regular text ", Normal "_not**line"]),
        p lineP "``\n" ~?= Right (S.Line [Normal "``"])
      ]

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
        p ulListP "- item 1\n-item 2\n" ~?= Right (UnorderedList [S.Line [Normal "item 1"], S.Line [Normal "item 2"]]),
        p ulListP "* item 1\n* item 2\n" ~?= Right (UnorderedList [S.Line [Normal "item 1"], S.Line [Normal "item 2"]])
      ]

test_olListP =
  "ordered list"
    ~: TestList
      [ p olListP "1.1" ~?= Left "No parses",
        p olListP "1. item 1\n" ~?= Right (OrderedList (1, [S.Line [Normal "item 1"]])),
        p olListP "1. item 1\n2. item 2\n" ~?= Right (OrderedList (1, [S.Line [Normal "item 1"], S.Line [Normal "item 2"]])),
        p olListP "11. item 1\n2. item 2\n" ~?= Right (OrderedList (11, [S.Line [Normal "item 1"], S.Line [Normal "item 2"]])),
        p olListP "1. item 1\n2.item 2\n"
          ~?= Right
            ( OrderedList (1, [S.Line [Normal "item 1"], S.Line [Normal "item 2"]])
            )
      ]

test_linkP =
  "link"
    ~: TestList
      [ p linkP "[()]\n" ~?= Left "No parses",
        p linkP "[google](google.com\n" ~?= Left "No parses",
        p linkP "[google]\n(google.com)\n" ~?= Left "No parses",
        p linkP "[google](google.com)\n" ~?= Right (Link [Normal "google"] "google.com"),
        p linkP "[](google.com)\n" ~?= Right (Link [] "google.com"),
        p linkP "[google]()\n" ~?= Right (Link [Normal "google"] "")
      ]

test_imgP =
  "link"
    ~: TestList
      [ p imgP "[google](google.com)\n" ~?= Left "No parses",
        p imgP "![google]\n(google.com)\n" ~?= Left "No parses",
        p imgP "![img](image.png)" ~?= Right (Image "img" "image.png"),
        p imgP "![](image.png)" ~?= Right (Image "" "image.png")
      ]

test_blockQuoteP =
  "block quote"
    ~: TestList
      [ p quoteP "``` ``" ~?= Left "No parses",
        p quoteP ">hello\n" ~?= Right (BlockQuote [S.Line [Normal "hello"]]),
        p quoteP ">1\n>2\n" ~?= Right (BlockQuote [S.Line [Normal "1"], S.Line [Normal "2"]]),
        p quoteP ">1\n>2\n> 3\n" ~?= Right (BlockQuote [S.Line [Normal "1"], S.Line [Normal "2"], S.Line [Normal "3"]])
      ]

test_paragraphP =
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
        p codeBlockP "```\n```\n" ~?= Right (CodeBlock ""),
        p codeBlockP "```\nhello!\n```\n" ~?= Right (CodeBlock "hello!\n"),
        p codeBlockP "```\na line\nanother line\n```\n"
          ~?= Right
            (CodeBlock "a line\nanother line\n")
      ]

test_brPHrP =
  "br and hr"
    ~: TestList
      [ p hrP "--" ~?= Left "No parses",
        p hrP "---" ~?= Right Hr,
        p hrP "--------" ~?= Right Hr,
        p brP "\n\n" ~?= Right Br,
        p brP "" ~?= Left "No parses",
        p brP " " ~?= Left "No parses"
      ]

test_tableP =
  "table"
    ~: TestList
      []

test_blockP =
  "parsing block"
    ~: TestList
      [ p blockP "# Heading 1 `code`\n `code`"
          ~?= Right (Heading 1 (S.Line [Normal "Heading 1 ", InlineCode "code"]))
      ]

test_markdownP =
  "markdown doc"
    ~: TestList
      [ p markdownP "# Heading 1\n"
          ~?= Right (Doc [Heading 1 (S.Line [Normal "Heading 1"])]),
        p markdownP sampleText
          ~?= Right
            (Doc [Heading 1 (Line [Normal "Heading 1"]), Paragraph (Line [Normal "This is ", InlineCode "inline code", Normal ". "]), Paragraph (Line [Bold "bold", Normal ", ", Italic "italic", Normal ", ", Strikethrough "struckthrough"]), CodeBlock "fold :: (a -> b -> b) -> b -> [a] -> b\nfold f z [] = z\nfold f z (x:xs) = f x (fold f z xs)\n", OrderedList (3, [Line [Normal "This is a numbered list."], Line [Normal "This is the second item."], Line [Normal "This is the third item."]])])
      ]

markdownTests =
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
      test_paragraphP,
      test_ulListP,
      test_olListP,
      test_linkP,
      test_imgP,
      test_blockQuoteP,
      test_brPHrP,
      test_tableP,
      test_blockP,
      test_markdownP
    ]

{- HTML TESTS -}
test_hBoldP =
  "bold"
    ~: TestList
      [ p hBoldP "<b>bold word</b>" ~?= Right (Bold "bold word"),
        p hBoldP "<b>not bold_" ~?= Left "No parses"
      ]

test_hItalicP =
  "italic"
    ~: TestList
      [ p hItalicP "<i>italic word</i>" ~?= Right (Italic "italic word"),
        p hItalicP "not bold</i>" ~?= Left "No parses"
      ]

test_hStrikeP =
  "strikethrough"
    ~: TestList
      [ p hStrikeP "<del>strikethrough</del>" ~?= Right (Strikethrough "strikethrough"),
        p hStrikeP "<del>not strike**" ~?= Left "No parses"
      ]

test_hInlineCodeP =
  "inline code"
    ~: TestList
      [ p hInlineCodeP "<code>regular code</code>" ~?= Right (InlineCode "regular code"),
        p hInlineCodeP "<code>special>char</code>" ~?= Right (InlineCode "special>char"),
        p hInlineCodeP "<code>not code" ~?= Left "No parses"
      ]

test_hNormalP =
  "normal text"
    ~: TestList
      [ p hNormalP "regular text" ~?= Right (Normal "regular text"),
        p hNormalP "special>char" ~?= Right (Normal "special>char"),
        p hNormalP "hello<p>world" ~?= Right (Normal "hello")
      ]

test_hLinkP =
  "link"
    ~: TestList
      [ p hLinkP "<a href=\"google.com\">google</a>" ~?= Right (Link [Normal "google"] "google.com"),
        p hLinkP "<a href=\"google.com\"></a>" ~?= Right (Link [] "google.com"),
        p hLinkP "<a>google</a>" ~?= Right (Link [Normal "google"] "")
      ]

test_hTextP =
  "text"
    ~: TestList
      [ p hTextP "regular text" ~?= Right (Normal "regular text"),
        p hTextP "<code>code</code>" ~?= Right (InlineCode "code"),
        p hTextP "<del>strikethrough</del>" ~?= Right (Strikethrough "strikethrough"),
        p hTextP "<b>bold</b>" ~?= Right (Bold "bold"),
        p hTextP "<i>italic</i>" ~?= Right (Italic "italic"),
        p hTextP "<code>code test this" ~?= Right (Normal "<code>code test this")
      ]

test_hLineP =
  "line"
    ~: TestList
      [ p hLineP "line" ~?= Right (S.Line [Normal "line"]),
        p hLineP "<code>code</code> line" ~?= Right (S.Line [InlineCode "code", Normal " line"]),
        p hLineP "<del>strike</del> <code>code</code> <b>bold</b> <i>italic</i> line"
          ~?= Right (S.Line [Strikethrough "strike", Normal " ", InlineCode "code", Normal " ", Bold "bold", Normal " ", Italic "italic", Normal " line"]),
        -- TODO: perhaps change this so it doesn't output 2 normals?
        p hLineP "<del>strike</del> <code>code</code> <i>line"
          ~?= Right (S.Line [Strikethrough "strike", Normal " ", InlineCode "code", Normal " ", Normal "<i>line"]),
        p hLineP "<code>code</code> **line"
          ~?= Right (S.Line [InlineCode "code", Normal " **line"]),
        p hLineP "regular text <not**line"
          ~?= Right (Line [Normal "regular text <not**line"]),
        p hLineP "``\n" ~?= Right (S.Line [Normal "``\n"])
      ]

test_hHeadingP =
  "heading"
    ~: TestList
      [ p hHeadingP "<h1>Heading 1</h1>" ~?= Right (Heading 1 (S.Line [Normal "Heading 1"])),
        p hHeadingP "<h4>Heading 4</h4>" ~?= Right (Heading 4 (S.Line [Normal "Heading 4"])),
        p hHeadingP "<h7>Heading 7</h7>" ~?= Left "No parses",
        p hHeadingP "Heading 1\n" ~?= Left "No parses",
        p hHeadingP "<h2><h1>Heading 2</h2>" ~?= Right (Heading 2 (S.Line [Normal "<h1>Heading 2"]))
      ]

-- test_ulListP =
--   "unordered list"
--     ~: TestList
--       [ p ulListP "-1\n" ~?= Left "No parses",
--         p ulListP "- item 1\n" ~?= Right (UnorderedList [S.Line [Normal "item 1"]]),
--         p ulListP "- item 1\ndontparsethis" ~?= Right (UnorderedList [S.Line [Normal "item 1"]]),
--         p ulListP "- item 1\n- item 2\n" ~?= Right (UnorderedList [S.Line [Normal "item 1"], S.Line [Normal "item 2"]]),
--         p ulListP "- item 1\n-item 2\n" ~?= Right (UnorderedList [S.Line [Normal "item 1"], S.Line [Normal "item 2"]]),
--         p ulListP "* item 1\n* item 2\n" ~?= Right (UnorderedList [S.Line [Normal "item 1"], S.Line [Normal "item 2"]])
--       ]

-- test_olListP =
--   "ordered list"
--     ~: TestList
--       [ p olListP "1.1" ~?= Left "No parses",
--         p olListP "1. item 1\n" ~?= Right (OrderedList (1, [S.Line [Normal "item 1"]])),
--         p olListP "1. item 1\n2. item 2\n" ~?= Right (OrderedList (1, [S.Line [Normal "item 1"], S.Line [Normal "item 2"]])),
--         p olListP "11. item 1\n2. item 2\n" ~?= Right (OrderedList (11, [S.Line [Normal "item 1"], S.Line [Normal "item 2"]])),
--         p olListP "1. item 1\n2.item 2\n"
--           ~?= Right
--             ( OrderedList (1, [S.Line [Normal "item 1"], S.Line [Normal "item 2"]])
--             )
--       ]

test_hImgP =
  "link"
    ~: TestList
      [ p hImgP "<img>" ~?= Left "No parses",
        p hImgP "<img alt=\"alternative\" src=\"url\">" ~?= Right (Image "alternative" "url"),
        p hImgP "<img alt=\"\" src=\"image.png\">" ~?= Right (Image "" "image.png")
      ]

-- test_blockQuoteP =
--   "block quote"
--     ~: TestList
--       [ p quoteP "``` ``" ~?= Left "No parses",
--         p quoteP ">hello\n" ~?= Right (BlockQuote [S.Line [Normal "hello"]]),
--         p quoteP ">1\n>2\n" ~?= Right (BlockQuote [S.Line [Normal "1"], S.Line [Normal "2"]]),
--         p quoteP ">1\n>2\n> 3\n" ~?= Right (BlockQuote [S.Line [Normal "1"], S.Line [Normal "2"], S.Line [Normal "3"]])
--       ]

-- test_paragraphP =
--   "paragraph"
--     ~: TestList
--       [ p paragraphP "regular string\n"
--           ~?= Right
--             (Paragraph (S.Line [Normal "regular string"])),
--         p paragraphP "regular string `code` and *italics*\n"
--           ~?= Right (Paragraph (S.Line [Normal "regular string ", InlineCode "code", Normal " and ", Italic "italics"]))
--       ]

-- test_codeBlockP =
--   "code block"
--     ~: TestList
--       [ p codeBlockP "``` ``\n" ~?= Left "No parses",
--         p codeBlockP "``````\n" ~?= Left "No parses",
--         p codeBlockP "```\n```\n" ~?= Right (CodeBlock ""),
--         p codeBlockP "```\nhello!\n```\n" ~?= Right (CodeBlock "hello!\n"),
--         p codeBlockP "```\na line\nanother line\n```\n"
--           ~?= Right
--             (CodeBlock "a line\nanother line\n")
--       ]

-- test_brPHrP =
--   "br and hr"
--     ~: TestList
--       [ p hrP "--" ~?= Left "No parses",
--         p hrP "---" ~?= Right Hr,
--         p hrP "--------" ~?= Right Hr,
--         p brP "\n\n" ~?= Right Br,
--         p brP "" ~?= Left "No parses",
--         p brP " " ~?= Left "No parses"
--       ]

-- test_tableP =
--   "table"
--     ~: TestList
--       []

-- test_blockP =
--   "parsing block"
--     ~: TestList
--       [ p blockP "# Heading 1 `code`\n `code`"
--           ~?= Right (Heading 1 (S.Line [Normal "Heading 1 ", InlineCode "code"]))
--       ]

htmlTests =
  TestList
    [ test_hBoldP,
      test_hItalicP,
      test_hStrikeP,
      test_hInlineCodeP,
      test_hNormalP,
      -- test_hLinkP,
      test_hTextP,
      test_hLineP,
      test_hHeadingP,
      test_hImgP
    ]

test_all = do
  print "RUNNING MARKDOWN TESTS"
  runTestTT markdownTests
  print "******************"
  print "RUNNING HTML TESTS"
  runTestTT htmlTests