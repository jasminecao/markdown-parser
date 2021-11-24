module HTMLParser where

import Data.Char (isSpace)
import Syntax (Block (..), Doc (Doc), Line, Text (..), reservedMarkdownChars)
import qualified Syntax as S
import Text.Parsec.Token
import Text.ParserCombinators.Parsec

