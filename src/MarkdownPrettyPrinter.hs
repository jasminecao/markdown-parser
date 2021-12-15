module MarkdownPrettyPrinter where

import Syntax (Block (..), Doc (Doc), Line (..), TableBody (..), TableCell (..), TableHead (..), TableRow (..), Text (..))
import qualified Syntax as S
import Text.PrettyPrint hiding (braces, parens, sep, (<>))
import qualified Text.PrettyPrint as PP

-- | Pretty print a document into Markdown
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
  pp (S.OrderedList (i, ls) level) = printOrderedListItem level i ls
    where
      printOrderedListItem :: Int -> Int -> [S.Block] -> PP.Doc
      printOrderedListItem level i [] = mempty
      printOrderedListItem level i (ol@(OrderedList _ _) : ls) = 
        pp ol <> printOrderedListItem level (i + 1) ls
      printOrderedListItem level i (ul@(UnorderedList _ _) : ls) = 
        pp ul <> printOrderedListItem level (i + 1) ls
      printOrderedListItem level i (l : ls) =
        PP.text (replicate level '\t') 
          <> PP.text (show i) 
          <> PP.text ". " 
          <> pp l 
          <> printOrderedListItem level (i + 1) ls
  pp (S.UnorderedList ls level) = printUnorderedListItem level ls
    where
      printUnorderedListItem :: Int -> [S.Block] -> PP.Doc
      printUnorderedListItem level [] = mempty
      printUnorderedListItem level (ol@(OrderedList _ _) : ls) = pp ol <> printUnorderedListItem level ls
      printUnorderedListItem level (ul@(UnorderedList _ _) : ls) = pp ul <> printUnorderedListItem level ls
      printUnorderedListItem level (l : ls) =
        PP.text (replicate level '\t') 
          <> PP.text "- " 
          <> pp l
          <> printUnorderedListItem level ls
  pp (S.Image alt src) = PP.text "!" <> PP.brackets (PP.text alt) <> PP.parens (PP.text src)
  pp (S.BlockQuote ls) = PP.hcat $ map ((PP.text ">" <>) . pp) ls
  pp (S.CodeBlock str) = PP.text "```\n" <> PP.text str <> PP.text "```"
  pp S.Hr = PP.text "---\n"
  pp S.Br = PP.text "\n\n"
  pp (S.Table thead tbody) = pp thead <> pp tbody

instance PP S.TableHead where
  pp (S.TableHead ls@(TableRow cells)) =
    pp ls <> PP.text "|"
      <> foldr (\x acc -> PP.text "---|" <> acc) PP.empty [1 .. length cells]
      <> PP.text "\n"

instance PP S.TableBody where
  pp (S.TableBody trs) = PP.hcat (map pp trs)

instance PP S.TableRow where
  pp (S.TableRow ls) = PP.text "|" <> PP.hcat (map pp ls) <> PP.text "\n"

instance PP S.TableCell where
  pp (S.TableCell (S.Line ts)) = PP.hcat (map pp ts) <> PP.text "|"

instance PP S.Line where
  pp (S.Line ts) = PP.hcat (map pp ts) <> PP.text "\n"

instance PP S.Text where
  pp (S.Link l href) = PP.brackets (PP.hcat (map pp l)) <> PP.parens (PP.text href)
  pp (S.Bold s) = PP.text "**" <> PP.text s <> PP.text "**"
  pp (S.Italic s) = PP.text "*" <> PP.text s <> PP.text "*"
  pp (S.Strikethrough s) = PP.text "~~" <> PP.text s <> PP.text "~~"
  pp (S.InlineCode s) = PP.text "`" <> PP.text s <> PP.text "`"
  pp (S.Normal s) = PP.text s