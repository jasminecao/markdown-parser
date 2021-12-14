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

noConsecutiveNormal :: [Text] -> Bool
noConsecutiveNormal [] = True
noConsecutiveNormal (Normal _ : Normal _ : _) = False
noConsecutiveNormal (_ : rest) = noConsecutiveNormal rest

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
      noInnerLink l = foldr (\x acc -> not (isLink x) && acc) True l && noConsecutiveNormal l

      isLink :: Text -> Bool
      isLink (Link _ _) = True
      isLink _ = False

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
      genUnorderedList = UnorderedList <$> QC.sized gen
        where
          gen :: Int -> Gen [S.Line]
          gen n =
            QC.frequency
              [ (1, fmap (: []) (arbitrary :: Gen S.Line)),
                (n, Monad.liftM2 (:) (arbitrary :: Gen S.Line) (gen (n `div` 2)))
              ]
      genBlockQuote = BlockQuote <$> QC.listOf1 arbitrary
      genHr = pure Hr
      genBr = pure Br
      genTable = Table <$> arbitrary <*> arbitrary

  shrink (Heading n ln) = Heading n <$> shrink ln
  shrink (Paragraph ln) = Paragraph <$> shrink ln
  shrink (OrderedList (i, ln)) = [OrderedList (i, ln') | ln' <- shrink ln, not (null ln')]
  shrink (UnorderedList ln) = [UnorderedList ln' | ln' <- shrink ln, not (null ln')]
  shrink (Image alt src) = Image alt <$> shrink src
  shrink (BlockQuote ln) = [BlockQuote ln' | ln' <- shrink ln, not (null ln')]
  shrink (CodeBlock ln) = CodeBlock <$> shrink ln
  shrink Hr = [Hr]
  shrink Br = [Br]
  shrink (Table thead tbody) = Table <$> shrink thead <*> shrink tbody

parsePrettyMD :: MarkdownPrettyPrinter.PP a1 => Parser a2 -> a1 -> Either ParseError a2
parsePrettyMD p x = parse p "" (markdownPretty x)

parsePrettyHTML :: HTMLPrettyPrinter.PP a1 => Parser a2 -> a1 -> Either ParseError a2
parsePrettyHTML p x = parse p "" (htmlPretty x)

prop_roundtrip_text :: Text -> Bool
prop_roundtrip_text t = parsePrettyMD textP t == Right t

prop_roundtrip_line :: S.Line -> Bool
prop_roundtrip_line l = parsePrettyMD lineP l == Right l

prop_roundtrip_block :: Block -> Bool
prop_roundtrip_block b = parsePrettyMD blockP b == Right b

prop_roundtrip_html_text :: Text -> Bool
prop_roundtrip_html_text t = parsePrettyHTML hTextP t == Right t

prop_roundtrip_html_line :: S.Line -> Bool
prop_roundtrip_html_line l = parsePrettyHTML hLineP l == Right l

prop_roundtrip_html_block :: Block -> Bool
prop_roundtrip_html_block b = parsePrettyHTML hBlockP b == Right b

prop_roundtrip_full_text :: Text -> Bool
prop_roundtrip_full_text t = case parsePrettyMD textP t of
  Right a -> parsePrettyHTML hTextP a == Right t
  Left _ -> False

prop_roundtrip_full_line :: S.Line -> Bool
prop_roundtrip_full_line l = case parsePrettyMD lineP l of
  Right a -> parsePrettyHTML hLineP a == Right l
  Left _ -> False

prop_roundtrip_full_block :: Block -> Bool
prop_roundtrip_full_block b = case parsePrettyMD blockP b of
  Right a -> parsePrettyHTML hBlockP b == Right b
  Left _ -> False

qc :: IO ()
qc = do
  putStrLn "roundtrip_text"
  QC.quickCheck prop_roundtrip_text
  putStrLn "roundtrip_line"
  QC.quickCheck prop_roundtrip_line
  putStrLn "roundtrip_html_text"
  QC.quickCheck prop_roundtrip_html_text
  putStrLn "roundtrip_html_line"
  QC.quickCheck prop_roundtrip_html_line
  putStrLn "roundtrip_full_text"
  QC.quickCheck prop_roundtrip_full_text
  putStrLn "roundtrip_full_line"
  QC.quickCheck prop_roundtrip_full_line
  putStrLn "roundtrip_full_block"
  QC.quickCheck prop_roundtrip_full_block