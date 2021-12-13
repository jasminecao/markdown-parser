module HUnitTests where

import Data.Char (isSpace)
import HTMLParser
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

test_hBoldP =
  "bold"
    ~: TestList
      [ p hBoldP "<b>bold word</b>" ~?= Right (Bold "bold word"),
        p hBoldP "<b>not bold word</bbb>" ~?= Left "No parses",
        p hBoldP "<b not bold word></b>" ~?= Left "No parses",
        p hBoldP "<b>bold word </b>" ~?= Right (Bold "bold word "),
        p hBoldP "<b>not bold word<b>" ~?= Left "No parses"
      ]

test_hItalicP =
  "italic"
    ~: TestList
      [ p hItalicP "<i>italic word</i>" ~?= Right (Italic "italic word"),
        p hItalicP "<b> bold</b>" ~?= Left "No parses",
        p hItalicP "<iii>also not italic</i>" ~?= Left "No parses"
      ]

test_hStrikeP =
  "strikethrough"
    ~: TestList
      [ p hStrikeP "<del>strikethrough</del>" ~?= Right (Strikethrough "strikethrough"),
        p hStrikeP "<del>not strike</b>" ~?= Left "No parses",
        p hStrikeP "<d>also not strike</d>" ~?= Left "No parses"
      ]

test_hInlineCodeP =
  "inline code"
    ~: TestList
      [ p hInlineCodeP "<code>regular code</code>" ~?= Right (InlineCode "regular code"),
        p hInlineCodeP "<code>f :: a -> b</code>" ~?= Right (InlineCode "f :: a -> b"),
        p hInlineCodeP "<code </code>" ~?= Left "No parses"
      ]

test_hNormalP =
  "normal text"
    ~: TestList
      [ p hNormalP "regular text" ~?= Right (Normal "regular text")
      ]

test_hTextP =
  "text"
    ~: TestList
      [ p hTextP "regular text" ~?= Right (Normal "regular text"),
        p hTextP "<code>code</code>" ~?= Right (InlineCode "code"),
        p hTextP "<del>strikethrough</del>" ~?= Right (Strikethrough "strikethrough"),
        p hTextP "<b>bold</b>" ~?= Right (Bold "bold"),
        p hTextP "<i>italic</i>" ~?= Right (Italic "italic"),
        p hTextP "<code> test this" ~?= Left "No parses"
      ]

{-test_hLineP =
  "line"
    ~: TestList
      [ p hLineP "line\n" ~?= Right (S.Line [Normal "line"]),
        p hLineP "`code` line\n" ~?= Right (S.Line [InlineCode "code", Normal " line"]),
        p hLineP "~~strike~~ `code` **bold** *italic* line\n"
          ~?= Right (S.Line [Strikethrough "strike", Normal " ", InlineCode "code", Normal " ", Bold "bold", Normal " ", Italic "italic", Normal " line"]),
        -- TODO: perhaps change this so it doesn't output 2 normals?
        p hLineP "~~strike~~ `code` *line\n"
          ~?= Right (S.Line [Strikethrough "strike", Normal " ", InlineCode "code", Normal " ", Normal "*line"]),
        p hLineP "`code` **line\nend"
          ~?= Right (S.Line [InlineCode "code", Normal " ", Normal "**line"]),
        p hLineP "regular text _not**line\n"
          ~?= Right (Line [Normal "regular text ", Normal "_not**line"]),
        p hLineP "``\n" ~?= Right (S.Line [Normal "``"])
      ]

test_hHeadingP =
  "heading"
    ~: TestList
      [ p hHeadingP "# Heading 1\n" ~?= Right (Heading 1 (S.Line [Normal "Heading 1"])),
        p hHeadingP "#### Heading 4\n" ~?= Right (Heading 4 (S.Line [Normal "Heading 4"])),
        p hHeadingP "####### Heading 7\n" ~?= Left "No parses",
        p hHeadingP "Heading 1\n" ~?= Left "No parses",
        p hHeadingP "## # Heading 2\n" ~?= Right (Heading 2 (S.Line [Normal "# Heading 2"]))
      ]

test_hUListP =
  "unordered list"
    ~: TestList
      [ p hUListP "-1\n" ~?= Left "No parses",
        p hUListP "- item 1\n" ~?= Right (UnorderedList [S.Line [Normal "item 1"]]),
        p hUListP "- item 1\ndontparsethis" ~?= Right (UnorderedList [S.Line [Normal "item 1"]]),
        p hUListP "- item 1\n- item 2\n" ~?= Right (UnorderedList [S.Line [Normal "item 1"], S.Line [Normal "item 2"]]),
        p hUListP "- item 1\n-item 2\n" ~?= Right (UnorderedList [S.Line [Normal "item 1"], S.Line [Normal "item 2"]]),
        p hUListP "* item 1\n* item 2\n" ~?= Right (UnorderedList [S.Line [Normal "item 1"], S.Line [Normal "item 2"]])
      ]

test_hOListP =
  "ordered list"
    ~: TestList
      [ p hOListP "1.1" ~?= Left "No parses",
        p hOListP "1. item 1\n" ~?= Right (OrderedList (1, [S.Line [Normal "item 1"]])),
        p hOListP "1. item 1\n2. item 2\n" ~?= Right (OrderedList (1, [S.Line [Normal "item 1"], S.Line [Normal "item 2"]])),
        p hOListP "11. item 1\n2. item 2\n" ~?= Right (OrderedList (11, [S.Line [Normal "item 1"], S.Line [Normal "item 2"]])),
        p hOListP "1. item 1\n2.item 2\n"
          ~?= Right
            ( OrderedList (1, [S.Line [Normal "item 1"], S.Line [Normal "item 2"]])
            )
      ]

test_hLinkP =
  "link"
    ~: TestList
      [ p hLinkP "[()]\n" ~?= Left "No parses",
        p hLinkP "[google](google.com\n" ~?= Left "No parses",
        p hLinkP "[google]\n(google.com)\n" ~?= Left "No parses",
        p hLinkP "[google](google.com)\n" ~?= Right (Link [Normal "google"] "google.com"),
        p hLinkP "[](google.com)\n" ~?= Right (Link [] "google.com"),
        p hLinkP "[google]()\n" ~?= Right (Link [Normal "google"] "")
      ]

test_hImgP =
  "link"
    ~: TestList
      [ p hImgP "[google](google.com)\n" ~?= Left "No parses",
        p hImgP "![google]\n(google.com)\n" ~?= Left "No parses",
        p hImgP "![img](image.png)" ~?= Right (Image "img" "image.png"),
        p hImgP "![](image.png)" ~?= Right (Image "" "image.png")
      ]

test_hBlockQuoteP =
  "block quote"
    ~: TestList
      [ p quoteP "``` ``" ~?= Left "No parses",
        p quoteP ">hello\n" ~?= Right (BlockQuote [S.Line [Normal "hello"]]),
        p quoteP ">1\n>2\n" ~?= Right (BlockQuote [S.Line [Normal "1"], S.Line [Normal "2"]]),
        p quoteP ">1\n>2\n> 3\n" ~?= Right (BlockQuote [S.Line [Normal "1"], S.Line [Normal "2"], S.Line [Normal "3"]])
      ]

test_hParagraphP =
  "paragraph"
    ~: TestList
      [ p hParagraphP "regular string\n"
          ~?= Right
            (Paragraph (S.Line [Normal "regular string"])),
        p hParagraphP "regular string `code` and *italics*\n"
          ~?= Right (Paragraph (S.Line [Normal "regular string ", InlineCode "code", Normal " and ", Italic "italics"]))
      ]

test_hCodehBlockP =
  "code block"
    ~: TestList
      [ p hCodehBlockP "``` ``\n" ~?= Left "No parses",
        p hCodehBlockP "``````\n" ~?= Left "No parses",
        p hCodehBlockP "```\n```\n" ~?= Right (CodeBlock ""),
        p hCodehBlockP "```\nhello!\n```\n" ~?= Right (CodeBlock "hello!\n"),
        p hCodehBlockP "```\na line\nanother line\n```\n"
          ~?= Right
            ( CodeBlock "a line\nanother line\n" )
      ]

test_hBrPhHrP =
  "br and hr"
    ~: TestList
      [ p hHrP "--" ~?= Left "No parses",
        p hHrP "---" ~?= Right Hr,
        p hHrP "--------" ~?= Right Hr,
        p hBrP "\n\n" ~?= Right Br,
        p hBrP "" ~?= Left "No parses",
        p hBrP " " ~?= Left "No parses"
      ]

test_hTableP =
  "table"
    ~: TestList
      []

test_hBlockP =
  "parsing block"
    ~: TestList
      [ p hBlockP "# Heading 1 `code`\n `code`"
          ~?= Right (Heading 1 (S.Line [Normal "Heading 1 ", InlineCode "code"]))
      ]

test_htmlP =
  "markdown doc"
    ~: TestList
      [ p htmlP "# Heading 1\n"
          ~?= Right (Doc [Heading 1 (S.Line [Normal "Heading 1"])]),
        p htmlP sampleText
          ~?= Right
            ( Doc
                [ Heading 1 (Line [Normal "Heading 1"]),
                  Paragraph (Line [Normal "This is ", InlineCode "inline code", Normal ". "]),
                  Paragraph (Line [Bold "bold", Normal ", ", Italic "italic", Normal ", ", Strikethrough "struckthrough"]),
                  -- CodeBlock [Line [Normal "fold :: (a -> b -> b) -> b -> [a] -> b"], Line [Normal "fold f z [] = z"], Line [Normal "fold f z (x:xs) = f x (fold f z xs)"]],
                  OrderedList (3, [Line [Normal "This is a numbered list."], Line [Normal "This is the second item."], Line [Normal "This is the third item."]])
                ]
            )
      ]-}

htmlTests = TestList
  [ test_hBoldP,
    test_hItalicP,
    test_hStrikeP,
    test_hInlineCodeP,
    test_hNormalP,
    test_hTextP
    -- test_hLineP,
    -- test_hHeadingP,
    -- test_hCodehBlockP,
    -- test_hParagraphP,
    -- test_hUListP,
    -- test_hOListP,
    -- test_hLinkP,
    -- test_hImgP,
    -- test_hBlockQuoteP,
    -- test_hBrPhHrP,
    -- test_hTableP,
    -- test_hBlockP,
    -- test_htmlP
  ]

test_all = runTestTT htmlTests