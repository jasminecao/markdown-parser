module MarkdownPrettyPrinter where

import Syntax (Block (..), Doc (Doc), Line, Text (..))
import qualified Syntax as S
import Text.PrettyPrint hiding (braces, parens, sep, (<>))
import qualified Text.PrettyPrint as PP

markdownPretty :: PP a => a -> String
markdownPretty = PP.render . pp

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
  pp (S.Image alt src) = PP.text "!" <> PP.brackets (PP.text alt) <> PP.parens (PP.text src)
  -- TODO: handle multiple lines in blockquote
  pp (S.BlockQuote ls) = undefined --PP.vcat (map pp ls)
  pp (S.CodeBlock ls) = undefined --PP.text "```\n" <> PP.space <> PP.text str <> PP.text "\n```"
  pp S.Hr = PP.text "---\n"
  pp S.Br = PP.text "\n"
  pp (S.Table ls) = undefined

instance PP S.Line where
  pp (S.Line ts) = PP.hcat (map pp ts) <> PP.text "\n"

instance PP S.Text where
  pp (S.Link l href) = PP.brackets (PP.hcat (map pp l)) <> PP.parens (PP.text href)
  pp (S.Bold s) = PP.text "**" <> PP.text s <> PP.text "**"
  pp (S.Italic s) = PP.text "*" <> PP.text s <> PP.text "*"
  pp (S.Strikethrough s) = PP.text "~~" <> PP.text s <> PP.text "~~"
  pp (S.InlineCode s) = PP.text "`" <> PP.text s <> PP.text "`"
  pp (S.Normal s) = PP.text s