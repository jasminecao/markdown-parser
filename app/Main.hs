module Main where

import HTMLPrettyPrinter
import Lib
import MarkdownParser
import qualified System.IO as IO
import qualified System.IO.Error as IO
import Text.Parsec.Token
import Text.ParserCombinators.Parsec as Parsec

main :: IO ()
main = do
  s <- readFile "somefile.txt"
  case parseMarkdown s of
    Left error -> print error
    Right doc -> IO.putStrLn $ pretty doc
  return ()

-- | parseFromFile p filePath runs a string parser p on the input
-- read from filePath using readFile. Returns either a
-- ParseError (Left) or a value of type a (Right).
-- parseFromFile :: Parser a -> String -> IO (Either ParseError a)
-- parseFromFile parser filename = do
--   IO.catchIOError
--     (do
--         handle <- IO.openFile filename IO.ReadMode
--         str <- IO.hGetContents handle
--         pure $ parse parser str)
--     (\e ->
--         pure $ Left $ "Error:" ++ show e)
