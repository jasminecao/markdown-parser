module Main where

import HTMLPrettyPrinter (htmlPretty)
-- import Lib
import MarkdownParser
import qualified System.IO as IO
import qualified System.IO.Error as IO
import Text.Parsec.Token
import Text.ParserCombinators.Parsec as Parsec

main :: IO ()
main = do
  putStrLn "What markdown file do you want to parse?"
  filename <- getLine
  s <- readFile (filename ++ ".md")
  case parseMarkdown s of
    Left error -> print error
    Right doc -> writeFile (filename ++ ".html") (htmlPretty doc) >> putStrLn "Parsed successfully!"
  return ()
