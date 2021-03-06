module MDHUnitTests where

import Data.Char (isSpace)
import HTMLParser
import MarkdownParser
import SampleText
import Syntax (Block (..), Doc (Doc), Line (..), TableBody (..), TableCell (..), TableHead (..), TableRow (..), Text (..))
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
      [ p (normalP ['\n']) "regular text" ~?= Right (Normal "regular text")
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
        p lineP "~~strike~~ `code` *line\n"
          ~?= Right (S.Line [Strikethrough "strike", Normal " ", InlineCode "code", Normal " *line"]),
        p lineP "`code` **line\nend"
          ~?= Right (S.Line [InlineCode "code", Normal " **line"]),
        p lineP "regular text _not**line\n"
          ~?= Right (Line [Normal "regular text _not**line"]),
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
        p ulListP "- item 1\n"
          ~?= Right (UnorderedList [Paragraph $ Line [Normal "item 1"]] 0),
        p ulListP "- item 1\ndontparsethis"
          ~?= Right (UnorderedList [Paragraph $ Line [Normal "item 1"]] 0),
        p ulListP "- item 1\n- item 2\n"
          ~?= Right (UnorderedList [Paragraph $ S.Line [Normal "item 1"], Paragraph $ S.Line [Normal "item 2"]] 0),
        p ulListP "- item 1\n-item 2\n"
          ~?= Right (UnorderedList [Paragraph $ S.Line [Normal "item 1"], Paragraph $ S.Line [Normal "item 2"]] 0),
        p ulListP "* item 1\n* item 2\n"
          ~?= Right (UnorderedList [Paragraph $ S.Line [Normal "item 1"], Paragraph $ S.Line [Normal "item 2"]] 0),
        p ulListP "- item 1\n\t- subitem 1\n"
          ~?= Right (UnorderedList [Paragraph $ S.Line [Normal "item 1"], UnorderedList [Paragraph $ S.Line [Normal "subitem 1"]] 1] 0),
        p ulListP "- item 1\n\t- subitem 1\n\t\t- subsubitem 1\n"
          ~?= Right
            ( UnorderedList
                [ Paragraph $ S.Line [Normal "item 1"],
                  UnorderedList
                    [ Paragraph $ S.Line [Normal "subitem 1"],
                      UnorderedList [Paragraph $ S.Line [Normal "subsubitem 1"]] 2
                    ]
                    1
                ]
                0
            ),
        p ulListP "- item 1\n\t- subitem 1\n- item 2\n"
          ~?= Right
            ( UnorderedList
                [ Paragraph $ S.Line [Normal "item 1"],
                  UnorderedList [Paragraph $ S.Line [Normal "subitem 1"]] 1,
                  Paragraph $ S.Line [Normal "item 2"]
                ]
                0
            )
      ]

test_olListP =
  "ordered list"
    ~: TestList
      [ p olListP "1.1" ~?= Left "No parses",
        p olListP "1. item 1\n" ~?= Right (OrderedList (1, [Paragraph $ S.Line [Normal "item 1"]]) 0),
        p olListP "1. item 1\n2. item 2\n" ~?= Right (OrderedList (1, [Paragraph $ S.Line [Normal "item 1"], Paragraph $ S.Line [Normal "item 2"]]) 0),
        p olListP "11. item 1\n2. item 2\n" ~?= Right (OrderedList (11, [Paragraph $ S.Line [Normal "item 1"], Paragraph $ S.Line [Normal "item 2"]]) 0),
        p olListP "1. item 1\n2.item 2\n"
          ~?= Right
            ( OrderedList (1, [Paragraph $ S.Line [Normal "item 1"], Paragraph $ S.Line [Normal "item 2"]]) 0
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
      [p tableP tableSimple ~?= Right (Table (TableHead (TableRow [TableCell (Line [Normal "Syntax "]), TableCell (Line [Normal "Description "])])) (TableBody [TableRow [TableCell (Line [Normal "Header "]), TableCell (Line [Normal "Title "])], TableRow [TableCell (Line [Normal "Paragraph "]), TableCell (Line [Normal "Text "])]]))]

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
            ( Doc
                [ Heading
                    1
                    ( S.Line [Normal "Heading 1"]
                    ),
                  Paragraph (S.Line [Normal "This is ", InlineCode "inline code", Normal ". "]),
                  Paragraph (S.Line [Bold "bold", Normal ", ", Italic "italic", Normal ", ", Strikethrough "struckthrough"]),
                  CodeBlock "fold :: (a -> b -> b) -> b -> [a] -> b\nfold f z [] = z\nfold f z (x:xs) = f x (fold f z xs)\n",
                  OrderedList (3, [Paragraph (S.Line [Normal "This is a numbered list."]), Paragraph (S.Line [Normal "This is the second item."]), Paragraph (S.Line [Normal "This is the third item."])]) 0
                ]
            )
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

test_md = runTestTT markdownTests
