module QCTests where

import qualified Control.Monad as Monad
import Data.Char (isSpace)
import HTMLParser
import HTMLPrettyPrinter
import MarkdownParser
import MarkdownPrettyPrinter
import Syntax (Block (..), Doc (Doc), Line (..), TableBody (..), TableCell (..), TableHead (..), TableRow (..), Text (..))
import qualified Syntax as S
import Test.HUnit
import Test.QuickCheck (Arbitrary, Gen, arbitrary, choose, oneof, shrink)
import qualified Test.QuickCheck as QC
import Text.Parsec.Token
import Text.ParserCombinators.Parsec as Parsec

instance Arbitrary Text where
  arbitrary =
    oneof
      [ Bold <$> genSafeString,
        Italic <$> genSafeString,
        Strikethrough <$> genSafeString,
        InlineCode <$> genSafeString,
        Link <$> genLink <*> genSafeString,
        Normal <$> genSafeString
      ]
    where
      genLink :: Gen [Text]
      genLink =
        QC.listOf1 arbitrary `QC.suchThat` noInnerLink

      noInnerLink :: [Text] -> Bool
      noInnerLink [] = True
      noInnerLink (Link _ _ : _) = False
      noInnerLink (Normal _ : Normal _ : _) = False
      noInnerLink (_ : xs) = noInnerLink xs

      genSafeString :: Gen String
      genSafeString =
        QC.listOf1 (QC.elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']))

  shrink (Bold str) = Bold <$> shrink str
  shrink (Italic str) = Italic <$> shrink str
  shrink (Strikethrough str) = Strikethrough <$> shrink str
  shrink (InlineCode str) = InlineCode <$> shrink str
  shrink (Link l str) = [Link l' str | l' <- shrink l]
  shrink (Normal str) = Normal <$> shrink str

instance Arbitrary S.Line where
  arbitrary = S.Line <$> genArbitraryLine
    where
      genArbitraryLine :: Gen [Text]
      genArbitraryLine =
        QC.listOf1 arbitrary
          `QC.suchThat` noConsecutiveNormal

      noConsecutiveNormal :: [Text] -> Bool
      noConsecutiveNormal [] = True
      noConsecutiveNormal (Normal _ : Normal _ : _) = False
      noConsecutiveNormal (_ : rest) = noConsecutiveNormal rest

  shrink (S.Line [x]) = S.Line <$> shrink [x]
  shrink (S.Line (x : xs)) = [S.Line xs]
  shrink _ = []

instance Arbitrary TableHead where
  arbitrary = TableHead <$> arbitrary
  shrink (TableHead r) = TableHead <$> shrink r

instance Arbitrary TableBody where
  arbitrary = TableBody <$> arbitrary
  shrink (TableBody rs) = TableBody <$> concat [shrink r | r <- shrink rs]

instance Arbitrary TableRow where
  arbitrary = TableRow <$> QC.sized gen
    where
      gen :: Int -> Gen [TableCell]
      gen n =
        QC.frequency
          [ (1, return []),
            (n, Monad.liftM2 (:) (arbitrary :: Gen TableCell) (gen (n `div` 2)))
          ]
  shrink (TableRow r) = TableRow <$> shrink r

instance Arbitrary TableCell where
  arbitrary = TableCell <$> arbitrary
  shrink (TableCell c) = TableCell <$> shrink c

instance Arbitrary Block where
  arbitrary =
    oneof
      [ genHeading,
        genParagraph,
        genOrderedList,
        genUnorderedList,
        genBlockQuote,
        genHr,
        genBr,
        genTable
      ]
    where
      genHeading = (Heading <$> choose (1, 6)) <*> arbitrary
      genParagraph = Paragraph <$> arbitrary
      genOrderedList = OrderedList <$> Monad.liftM2 (,) ((arbitrary :: Gen Int) `QC.suchThat` (>= 0)) (QC.listOf1 arbitrary)
      genUnorderedList = UnorderedList <$> QC.listOf1 arbitrary
      genBlockQuote = BlockQuote <$> QC.listOf1 arbitrary
      genHr = pure Hr
      genBr = pure Br
      genTable = Table <$> arbitrary <*> arbitrary

  shrink (Heading n ln) = Heading n <$> shrink ln
  shrink (Paragraph ln) = Paragraph <$> shrink ln
  shrink (OrderedList (i, ln)) = [OrderedList (i, ln') | ln' <- shrink ln, not (null ln')]
  shrink (UnorderedList ln) = [UnorderedList ln' | ln' <- shrink ln, not (null ln')]
  shrink (Image alt src) = Image alt <$> shrink src
  -- TODO: shrink BlockQuote
  shrink (BlockQuote ln) = undefined --[BlockQuote ln' | ln' <- shrink ln, not (null ln'), not]
  shrink (CodeBlock ln) = CodeBlock <$> shrink ln
  shrink Hr = [Hr]
  shrink Br = [Br]
  shrink (Table thead tbody) = Table <$> shrink thead <*> shrink tbody

-- TODO: abstract this out
prop_roundtrip_text :: Text -> Bool
prop_roundtrip_text t = parse textP "" (markdownPretty t) == Right t

prop_roundtrip_line :: S.Line -> Bool
prop_roundtrip_line l = parse lineP "" (markdownPretty l) == Right l

prop_roundtrip_block :: Block -> Bool
prop_roundtrip_block b = parse blockP "" (markdownPretty b) == Right b

prop_roundtrip_html_text :: Text -> Bool
prop_roundtrip_html_text t = parse hTextP "" (htmlPretty t) == Right t

prop_roundtrip_html_line :: S.Line -> Bool
prop_roundtrip_html_line l = parse hLineP "" (htmlPretty l) == Right l

prop_roundtrip_html_block :: Block -> Bool
prop_roundtrip_html_block b = parse hBlockP "" (htmlPretty b) == Right b

prop_roundtrip_full_text :: Text -> Bool
prop_roundtrip_full_text t = case parse textP "" (markdownPretty t) of
  Right a -> parse hTextP "" (htmlPretty a) == Right t
  Left _ -> False

prop_roundtrip_full_line :: S.Line -> Bool
prop_roundtrip_full_line l = case parse lineP "" (markdownPretty l) of
  Right l -> parse hLineP "" (htmlPretty l) == Right l
  Left _ -> False

prop_roundtrip_full_block :: Block -> Bool
prop_roundtrip_full_block b = case parse blockP "" (markdownPretty b) of
  Right b -> parse hBlockP "" (htmlPretty b) == Right b
  Left _ -> False

qc :: IO ()
qc = do
  putStrLn "roundtrip_text"
  QC.quickCheck prop_roundtrip_text
  putStrLn "roundtrip_line"
  QC.quickCheck prop_roundtrip_line
  -- putStrLn "roundtrip_block"
  -- QC.quickCheck prop_roundtrip_block
  putStrLn "roundtrip_html_text"
  QC.quickCheck prop_roundtrip_html_text
  putStrLn "roundtrip_html_line"
  QC.quickCheck prop_roundtrip_html_line
  -- putStrLn "roundtrip_html_block"
  -- QC.quickCheck prop_roundtrip_html_block
  putStrLn "roundtrip_full_text"
  QC.quickCheck prop_roundtrip_full_text
  putStrLn "roundtrip_full_line"
  QC.quickCheck prop_roundtrip_full_line
  putStrLn "roundtrip_full_block"
  QC.quickCheck prop_roundtrip_full_block