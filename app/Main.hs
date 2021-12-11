module Main where

import HTMLPrettyPrinter (htmlPretty)
import Lib
import MarkdownParser
import qualified System.IO as IO
import qualified System.IO.Error as IO
import Text.Parsec.Token
import Text.ParserCombinators.Parsec as Parsec

main :: IO ()
main = do
  s <- readFile "somefile.md"
  case parseMarkdown s of
    Left error -> print error
    Right doc -> IO.putStrLn $ htmlPretty doc
  return ()
