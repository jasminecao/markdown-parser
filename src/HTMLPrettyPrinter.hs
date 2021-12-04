module HTMLPrettyPrinter where

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
  pp (S.Heading n l) = tag ("h" ++ show n) (pp l)
  pp (S.Paragraph l) = tag "p" (pp l)
  pp (S.OrderedList (startVal, ls)) = 
    tagWithAttrs "ol" [("start", show startVal)] (PP.cat $ map (tag "li" . pp) ls)
  pp (S.UnorderedList ls) = tag "ul" (PP.cat $ map (tag "li" . pp) ls)
  pp (S.Link href l) = tagWithAttrs "a" [("href", href)] (pp l)
  -- TODO: handle multiple lines in blockquote
  pp (S.BlockQuote ls) = tag "blockquote" (PP.cat $ map (tag "p" . pp) ls)
  pp (S.CodeBlock str) = tag "pre" $ tag "code" (PP.cat $ map pp str)
  pp S.Hr = PP.text "<hr>"
  pp S.Br = PP.text "<br>"
  pp (S.Table ls) = undefined

instance PP S.Line where
  pp (S.Line ts) = PP.cat (map pp ts)

instance PP S.Text where
  pp (S.Bold s) = tag "b" $ PP.text s
  pp (S.Italic s) = tag "i" $ PP.text s
  pp (S.Strikethrough s) = tag "del" $ PP.text s
  pp (S.InlineCode s) = tag "code" $ PP.text s
  pp (S.Normal s) = PP.text s -- TODO: maybe <span>?

tag :: String -> PP.Doc -> PP.Doc
tag t = tagWithAttrs t []
  -- PP.text "<" <> PP.text t <> PP.text ">" <> context <> PP.text "</" <> PP.text t <> PP.text ">"

tagWithAttrs :: String -> [(String, String)] -> PP.Doc -> PP.Doc
tagWithAttrs t attrs context = 
  PP.text "<" <> PP.text t
    <> tagWithAttrInner t attrs context
  where
    tagWithAttrInner t [] context = 
      PP.text ">" <> 
      context <> 
      PP.text "</" <> PP.text t <> PP.text ">"
    tagWithAttrInner t ((attrName, attrVal):tl) context = -- attrName = attrVal
      PP.text " " <>
      PP.text attrName <> PP.text "=\"" <> PP.text attrVal <> PP.text "\"" <> 
      tagWithAttrInner t tl context