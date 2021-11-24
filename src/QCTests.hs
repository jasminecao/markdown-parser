module QCTests where

import Data.Char (isSpace)
import Syntax (Block (..), Doc (Doc), Line, Text (..), TableType (..), reservedMarkdownChars)
import qualified Syntax as S
import qualified Control.Monad as Monad
import Text.Parsec.Token
import Text.ParserCombinators.Parsec as Parsec
import Test.HUnit
import Test.QuickCheck (Arbitrary, Gen, arbitrary, shrink, oneof, choose)

-- class PP a where
--   pp :: a -> Content

instance Arbitrary Text where
    arbitrary = 
        oneof
            [ Bold <$> arbitrary,
              Italic <$> arbitrary,
              Underline <$> arbitrary,
              Strikethrough <$> arbitrary,
              InlineCode <$> arbitrary,
              Normal <$> arbitrary
            ]

    shrink (Bold str) = Bold <$> shrink str
    shrink (Italic str) = Italic <$> shrink str
    shrink (Underline str) = Underline <$> shrink str
    shrink (Strikethrough str) = Strikethrough <$> shrink str
    shrink (InlineCode str) = InlineCode <$> shrink str
    shrink (Normal str) = Normal <$> shrink str

instance Arbitrary S.Line where
    arbitrary = S.Line <$> arbitrary

    shrink (S.Line (x:xs)) = [S.Line xs]
    shrink _ = []

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

-- prop_roundtrip_val :: Value -> Bool
-- prop_roundtrip_val v = P.parse valueP (pretty v) == Right v

-- prop_roundtrip_exp :: Expression -> Bool
-- prop_roundtrip_exp e = P.parse expP (pretty e) == Right e

-- prop_roundtrip_stat :: Statement -> Bool
-- prop_roundtrip_stat s = P.parse statementP (pretty s) == Right s