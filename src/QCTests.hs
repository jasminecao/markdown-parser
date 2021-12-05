module QCTests where

import qualified Control.Monad as Monad
import Data.Char (isSpace)
import MarkdownParser
import MarkdownPrettyPrinter
import Syntax (Block (..), Doc (Doc), Line, TableType (..), Text (..), reservedMarkdownChars)
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
        Normal <$> genSafeString
      ]
    where
      genSafeString :: Gen String
      genSafeString =
        QC.listOf1 (QC.elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ [' ']))

  -- ( (arbitrary :: Gen Char)
  --     `QC.suchThat` (`notElem` reservedMarkdownChars)
  -- )

  shrink (Bold str) = Bold <$> shrink str
  shrink (Italic str) = Italic <$> shrink str
  shrink (Strikethrough str) = Strikethrough <$> shrink str
  shrink (InlineCode str) = InlineCode <$> shrink str
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

-- shrink (S.Line [x]) = S.Line <$> shrink [x]
-- shrink (S.Line (x : xs)) = [S.Line xs]
-- shrink _ = []

instance Arbitrary TableType where
  arbitrary =
    oneof
      [ TableHead <$> arbitrary,
        TableBody <$> arbitrary,
        TableRow <$> arbitrary,
        TableCell . S.Line <$> arbitrary
      ]

  shrink (TableHead b) = b
  shrink (TableBody r) = r
  shrink (TableRow c) = c
  shrink (TableCell c) = []

instance Arbitrary Block where
  arbitrary =
    oneof
      [ genHeading,
        genParagraph,
        genOrderedList,
        genUnorderedList,
        genLink,
        genBlockQuote,
        genHr,
        genBr,
        genTable
      ]
    where
      genHeading = (Heading <$> choose (1, 6)) <*> arbitrary
      genParagraph = Paragraph <$> arbitrary
      genOrderedList = OrderedList <$> arbitrary
      genUnorderedList = UnorderedList <$> arbitrary
      genLink = (Link <$> arbitrary) <*> arbitrary
      genBlockQuote = BlockQuote <$> arbitrary
      genHr = pure Hr
      genBr = pure Br
      genTable = arbitrary

  shrink (Heading n ln) = Heading n <$> shrink ln
  shrink (Paragraph ln) = Paragraph <$> shrink ln
  shrink (OrderedList ln) = OrderedList <$> shrink ln

prop_roundtrip_text :: Text -> Bool
prop_roundtrip_text t = parse textP "" (pretty t) == Right t

prop_roundtrip_line :: S.Line -> Bool
prop_roundtrip_line l = parse lineP "" (pretty l) == Right l

prop_roundtrip_block :: Block -> Bool
prop_roundtrip_block b = parse blockP "" (pretty b) == Right b