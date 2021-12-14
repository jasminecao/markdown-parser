module HTMLHUnitTests where

import Data.Char (isSpace)
import HTMLParser
import SampleText
import Syntax (Block (..), Doc (Doc), Line (..), TableBody, TableCell, TableHead, TableRow, Text (..))
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
      [ p hNormalP "regular text" ~?= Right (Normal "regular text"),
        p hNormalP "special>char" ~?= Right (Normal "special>char"),
        p hNormalP "hello<p>world" ~?= Right (Normal "hello")
      ]

test_hTextP =
  "text"
    ~: TestList
      [ p hTextP "regular text" ~?= Right (Normal "regular text"),
        p hTextP "<code>code</code>" ~?= Right (InlineCode "code"),
        p hTextP "<del>strikethrough</del>" ~?= Right (Strikethrough "strikethrough"),
        p hTextP "<b>bold</b>" ~?= Right (Bold "bold"),
        p hTextP "<i>italic</i>" ~?= Right (Italic "italic"),
        p hTextP "<code> test this" ~?= Right (Normal "<code> test this")
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
        p hLineP "``\n" ~?= Right (S.Line [Normal "``\n"]),
        p hLineP "<a>link</a> and normal stuff"
          ~?= Right (S.Line [Link [Normal "link"] "", Normal " and normal stuff"]),
        p hLineP "normal stuff and a <a href=\"url\">link</a>"
          ~?= Right (S.Line [Normal "normal stuff and a ", Link [Normal "link"] "url"])
      ]

test_hHeadingP =
  "heading"
    ~: TestList
      [ p hHeadingP "<h1>Heading 1</h1>" ~?= Right (Heading 1 (S.Line [Normal "Heading 1"])),
        p hHeadingP "<h4>Heading 4</h4>" ~?= Right (Heading 4 (S.Line [Normal "Heading 4"])),
        p hHeadingP "<h7>Heading 7</h7>" ~?= Left "No parses",
        p hHeadingP "Heading 1\n" ~?= Left "No parses",
        p hHeadingP "<h2><h1>Heading 2</h2>" ~?= Right (Heading 2 (S.Line [Normal "<h1>Heading 2"])),
        p hHeadingP "<h1><a href=\"url\">heading link</a></h1>"
          ~?= Right (Heading 1 (S.Line [Link [Normal "heading link"] "url"])),
        p hHeadingP "<h1><a href=\"url\">heading link</a> and normal stuff</h1>"
          ~?= Right (Heading 1 (S.Line [Link [Normal "heading link"] "url", Normal " and normal stuff"])),
        p hHeadingP "<h1>HEADING<a href=\"url\"> ONE</a></h1>"
          ~?= Right (Heading 1 (S.Line [Normal "HEADING", Link [Normal " ONE"] "url"]))
      ]

test_hUlListP =
  "unordered list"
    ~: TestList
      [ p hUlListP "<ul>1</ul>" ~?= Left "No parses",
        p hUlListP "<ul><li>item 1</li></ul>" ~?= Right (UnorderedList [S.Line [Normal "item 1"]]),
        p hUlListP "<ul><li>item 1</li></ul>dontparsethis" ~?= Right (UnorderedList [S.Line [Normal "item 1"]]),
        p hUlListP "<ul><li>item 1</li><li>item 2</li></ul>" ~?= Right (UnorderedList [S.Line [Normal "item 1"], S.Line [Normal "item 2"]])
      ]

test_hOlListP =
  "ordered list"
    ~: TestList
      [ p hOlListP "<ol start=\"1\"><li>item 1</li></ol>" ~?= Right (OrderedList (1, [S.Line [Normal "item 1"]])),
        p hOlListP "<ol start=\"1\"><li>item 1</li><li>item 2</li></ol>" ~?= Right (OrderedList (1, [S.Line [Normal "item 1"], S.Line [Normal "item 2"]])),
        p hOlListP "<ol start=\"11\"><li>item 1</li><li>item 2</li></ol>" ~?= Right (OrderedList (11, [S.Line [Normal "item 1"], S.Line [Normal "item 2"]]))
      ]

test_hImgP =
  "link"
    ~: TestList
      [ p hImgP "<img>" ~?= Left "No parses",
        p hImgP "<img alt=\"alternative\" src=\"url\">" ~?= Right (Image "alternative" "url"),
        p hImgP "<img alt=\"\" src=\"image.png\">" ~?= Right (Image "" "image.png")
      ]

test_hBlockQuoteP =
  "block quote"
    ~: TestList
      [ p hQuoteP "<blockquote><p>hello</p></blockquote>" ~?= Right (BlockQuote [S.Line [Normal "hello"]]),
        p hQuoteP "<blockquote><p>1</p><p>2</p></blockquote>" ~?= Right (BlockQuote [S.Line [Normal "1"], S.Line [Normal "2"]]),
        p hQuoteP "<blockquote><p>1</p><p>2</p><p>3</p></blockquote>" ~?= Right (BlockQuote [S.Line [Normal "1"], S.Line [Normal "2"], S.Line [Normal "3"]]),
        p hQuoteP "<blockquote></blockquote>" ~?= Right (BlockQuote [])
      ]

test_hParagraphP =
  "paragraph"
    ~: TestList
      [ p hParagraphP "<p>regular string</p>"
          ~?= Right
            (Paragraph (S.Line [Normal "regular string"])),
        p hParagraphP "<p>regular string <code>code</code> and <i>italics</i></p>"
          ~?= Right (Paragraph (S.Line [Normal "regular string ", InlineCode "code", Normal " and ", Italic "italics"]))
      ]

test_hCodeBlockP =
  "code block"
    ~: TestList
      [ p hCodeBlockP "<pre><code></code></pre>" ~?= Right (CodeBlock ""),
        p hCodeBlockP "<pre><code>hello!\n</code></pre>" ~?= Right (CodeBlock "hello!\n"),
        p hCodeBlockP "<pre><code>a line\nanother line\n</code></pre>"
          ~?= Right
            (CodeBlock "a line\nanother line\n")
      ]

test_hBrPhHrP =
  "br and hr"
    ~: TestList
      [ p hHrP "<hr/>" ~?= Right Hr,
        p hHrP "<hr>" ~?= Right Hr,
        p hBrP "<br/>" ~?= Right Br,
        p hBrP "<br>" ~?= Right Br
      ]

test_hTableP =
  "table"
    ~: TestList
      [ p hTableCellP "<td>abc</td>" ~?= Right (S.TableCell (S.Line [Normal "abc"])),
        p hTableRowP "<tr><td>abc</td></tr>" ~?= Right (S.TableRow [S.TableCell (S.Line [Normal "abc"])]),
        p hTableRowP "<tr><td>abc</td><td>def</td></tr>"
          ~?= Right (S.TableRow [S.TableCell (S.Line [Normal "abc"]), S.TableCell (S.Line [Normal "def"])]),
        p hTableRowP "<tr><td>abc</td><td>def</td></tr>"
          ~?= Right (S.TableRow [S.TableCell (S.Line [Normal "abc"]), S.TableCell (S.Line [Normal "def"])]),
        p hTableBodyP "<tbody><tr><td>abc</td><td>def</td></tr></tbody>"
          ~?= Right (S.TableBody [S.TableRow [S.TableCell (S.Line [Normal "abc"]), S.TableCell (S.Line [Normal "def"])]]),
        p hTableBodyP "<tbody><tr><td>abc</td></tr><tr><td>def</td></tr></tbody>"
          ~?= Right (S.TableBody [S.TableRow [S.TableCell (S.Line [Normal "abc"])], S.TableRow [S.TableCell (S.Line [Normal "def"])]]),
        p hTableHeadP "<thead><tr><td>abc</td><td>def</td></tr></thead>"
          ~?= Right (S.TableHead $ S.TableRow [S.TableCell (S.Line [Normal "abc"]), S.TableCell (S.Line [Normal "def"])]),
        p hTableBodyP "<thead><tr><td>abc</td></tr><tr><td>def</td></tr></thead>" ~?= Left "No parses",
        p
          hTableP
          "<table>\
          \<thead><tr><td>abc</td></tr></thead>\
          \<tbody><tr><td>def</td></tr></tbody>\
          \</table>"
          ~?= Right (Table (S.TableHead $ S.TableRow [S.TableCell $ S.Line [Normal "abc"]]) (S.TableBody [S.TableRow [S.TableCell $ S.Line [Normal "def"]]])),
        p
          hTableP
          "<table>\
          \<thead><tr><td>col1</td></tr></thead>\
          \<tbody>\
          \<tr><td>data1</td></tr>\
          \<tr><td>data2</td></tr>\
          \<tr><td>data3</td></tr>\
          \</tbody>\
          \</table>"
          ~?= Right (Table (S.TableHead (S.TableRow [S.TableCell (S.Line [S.Normal "col1"])])) (S.TableBody [S.TableRow [S.TableCell (S.Line [S.Normal "data1"])], S.TableRow [S.TableCell (S.Line [S.Normal "data2"])], S.TableRow [S.TableCell (S.Line [S.Normal "data3"])]]))
      ]

test_hBlockP =
  "parsing block"
    ~: TestList
      [ p hBlockP "<h1>Heading 1<code>code</code></h1>"
          ~?= Right (Heading 1 (S.Line [Normal "Heading 1", InlineCode "code"]))
      ]

test_hHtmlP =
  "html"
    ~: TestList
      [ p htmlP "<html><h1>Heading 1</h1><p><i>italics</i></p></html>"
          ~?= Right
            ( Doc [Heading 1 (S.Line [Normal "Heading 1"]), Paragraph (S.Line [Italic "italics"])]
            )
      ]

htmlTests =
  TestList
    [ test_hBoldP,
      test_hItalicP,
      test_hStrikeP,
      test_hInlineCodeP,
      test_hNormalP,
      test_hTextP,
      test_hLineP,
      test_hHeadingP,
      test_hCodeBlockP,
      test_hParagraphP,
      test_hUlListP,
      test_hOlListP,
      -- test_hLinkP,
      test_hImgP,
      test_hBlockQuoteP,
      test_hBrPhHrP,
      -- test_hTableP,
      test_hBlockP,
      test_hHtmlP
    ]

test_html = runTestTT htmlTests