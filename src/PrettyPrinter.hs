module PrettyPrinter where

import Syntax (Block (..), Doc (Doc), Line, Text (..))
import qualified Syntax as S
import Text.PrettyPrint hiding (braces, parens, sep, (<>))
import qualified Text.PrettyPrint as PP

pretty :: PP a => a -> String
pretty = PP.render . pp

class PP a where
  pp :: a -> PP.Doc

instance PP S.Doc where
  pp (S.Doc bs) = PP.vcat $ map pp bs

instance PP S.Block where
  pp (S.Heading n l) =
    foldr (\x acc -> PP.text "#" <> acc) PP.empty [1 .. n]
      <> PP.space
      <> pp l
  pp (S.Paragraph l) = pp l
  pp (S.OrderedList ls) = undefined
  pp (S.UnorderedList ls) = undefined
  pp (S.Link str l) = PP.parens $ PP.text str <> PP.brackets (pp l)
  -- TODO: handle multiple lines in blockquote
  pp (S.BlockQuote ls) = undefined
  pp (S.CodeBlock str) =  undefined -- PP.text "```" <> PP.space <> PP.text str <> PP.text "```"
  pp S.Hr = PP.text "---"
  pp S.Br = PP.text "\n"
  pp (S.Table ls) = undefined

instance PP S.Line where
  pp (S.Line ts) = PP.vcat $ map pp ts

instance PP S.Text where
  pp (S.Bold s) = PP.text "**" <> PP.text s <> PP.text "**"
  pp (S.Italic s) = PP.text "*" <> PP.text s <> PP.text "*"
  pp (S.Strikethrough s) = PP.text "~~" <> PP.text s <> PP.text "~~"
  pp (S.InlineCode s) = PP.text "`" <> PP.text s <> PP.text "`"
  pp (S.Normal s) = PP.text s