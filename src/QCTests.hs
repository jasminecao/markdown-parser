module QCTests where

import MarkdownParser
import SampleText
import Syntax (Content (..), EType (..), reservedMarkdownChars)
import Test.HUnit
import Text.Parsec.Token
import Text.ParserCombinators.Parsec
import qualified Test.QuickCheck

class PP a where
  pp :: a -> Content

instance Arbitrary Content where
    arbitrary = 
        frequency
            [ (3, return Empty),
              (4, Text <$> arbitrary),
              (3, (E <$> arbitrary) <$> arbitrary)
            ]

    shrink Empty = []
    shrink Text _ = []
    shrink (E etype content) = [content]

instance Arbitrary EType where
  arbitrary = oneof [genHeading, ]
    where
      genHeading = Heading <$> choose (1, 6)
      genParagraph = return Paragraph
      genOrderedList = OrderedList <$> arbitrary
      genUnorderedList = UnorderedList <$> arbitrary
      genLItem = return LItem
      genLink = Link <$> arbitrary
      genContainer = return Container
      genHead = return Head
      genBody = return Body
      genBlockQuote = return BlockQuote
      genHr = return Hr
      genBr = return Br
      genBold = return Bold
      genItalic = return Italic
      genUnderline = return Underline
      genStrikethrough = return Strikethrough
      genInlineCode = 

  shrink (IntVal n) = IntVal <$> shrink n
  shrink (BoolVal b) = BoolVal <$> shrink b
  shrink NilVal = []
  shrink (StringVal s) = StringVal <$> shrinkStringLit s
  shrink (TableVal _) = []

prop_roundtrip_val :: Value -> Bool
prop_roundtrip_val v = P.parse valueP (pretty v) == Right v

prop_roundtrip_exp :: Expression -> Bool
prop_roundtrip_exp e = P.parse expP (pretty e) == Right e

prop_roundtrip_stat :: Statement -> Bool
prop_roundtrip_stat s = P.parse statementP (pretty s) == Right s