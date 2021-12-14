# Markdown to HTML Parser

Jasmine Cao (`jcao3`)
Audrey Yang (`auyang`)

This project creates a parser which translates Markdown into HTML, and converts HTML into Markdown (for testing purposes).

## Module organization

#### `app`
This directory contains the entry point for our executable in [Main.hs](app/Main.hs), which has a method that prompts the user for a .md file name and parses that file into HTML.

#### `src`
We developed reusable libraries in this directory. 
- `HTMLParser.hs` contains all of our parsers for HTML plus any helpers that are specific to parsing HTML
- `HTMLPrettyPrinter.hs` contains HMTL PrettyPrint instances of our abstract syntax
- `Lib.hs` contains reusable general parser functions used by for parsing both HTML and Markdown
- `MarkdownParser.hs` contains all of our parsers for Markdown plus any helpers that are specific to parsing Markdown
- `MarkdownPrettyPrinter.hs` contains Markdown PrettyPrint instances of our abstract syntax
- `Syntax.hs` contains all our abstract representation syntax, plus reserved characters

#### `test`
We wrote HUnit and QuickCheck tests in this directory.

- `MDHUnitTests.hs` contains unit tests for each markdown parser element to ensure that they are translated correctly into our AST elements.
- `HTMLHUnitTests.hs` contains unit tests for each HTML parser.
- `SampleText.hs` contains sample text test cases for larger blocks of text.
- `QCTests.hs` contains our quickCheck tests with arbitrary generators for each of our AST elements and roundtrip properties to test our Markdown Parsers and HTML Pretty Printing. This includes properties for AST => Markdown Pretty Print => Markdown Parse => AST, AST -> HTML Pretty Print -> HTML Parse -> AST, and AST -> Markdown Pretty Print -> Markdown Parse -> AST -> HTML Pretty Print -> HTML Parse -> AST.

## Order to Read Components

1. Syntax
1. MarkdownParser
1. HTMLParser
1. MarkdownPrettyPrinter
1. HTMLPrettyPrinter
1. MDHUnitTests
1. HTMLHUnitTests
1. QCTests

## Building, running, and testing

This project compiles with `stack build`. 

You can run the main executable with `stack run`.
You can run the tests with `stack test`. 

Lastly, you can start a REPL with `stack ghci`.

## Additional Libraries
- parsec
- pretty