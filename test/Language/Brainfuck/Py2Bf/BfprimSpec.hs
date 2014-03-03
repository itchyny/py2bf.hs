{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Brainfuck.Py2Bf.BfprimSpec where
import Test.Hspec
import Test.QuickCheck
import Control.Monad (ap)
import Language.Brainfuck.Py2Bf.Bfprim

instance Arbitrary Bfprim where
  arbitrary = sized go
    where
      go 0 = elements [BfIncr, BfDecr, BfNext, BfPrev, BfPut, BfGet]
      go n = oneof [ go 0, return BfWhile `ap` listOf (go (n `div` 8)) ]

spec :: Spec
spec =
  describe "Bfprim" $ do
    readSpec
    readListSpec
    readShowSpec
    isBfprimNonIOSpec
    isBfWhileSpec

readSpec :: Spec
readSpec =
  it "reads Bfprim correctly" $ do
    read "+" `shouldBe` BfIncr
    read "-" `shouldBe` BfDecr
    read ">" `shouldBe` BfNext
    read "<" `shouldBe` BfPrev
    read "." `shouldBe` BfPut
    read "," `shouldBe` BfGet
    read "[]" `shouldBe` BfWhile []
    read "[+]" `shouldBe` BfWhile [BfIncr]
    read "[-]" `shouldBe` BfWhile [BfDecr]

readListSpec :: Spec
readListSpec =
  it "reads [Bfprim] correctly" $ do
    read "+++" `shouldBe` [BfIncr, BfIncr, BfIncr]
    read "+-+" `shouldBe` [BfIncr, BfDecr, BfIncr]
    read "+-><.," `shouldBe` [BfIncr, BfDecr, BfNext, BfPrev, BfPut, BfGet]
    read "[]" `shouldBe` [BfWhile []]
    read "+++[>+++<-]>." `shouldBe`
      [ BfIncr, BfIncr, BfIncr,
        BfWhile [ BfNext, BfIncr, BfIncr, BfIncr, BfPrev, BfDecr ],
        BfNext, BfPut ]
    read "[-]" `shouldBe` [BfWhile [BfDecr]]

readShowSpec :: Spec
readShowSpec =
  it "read . show == id" $
    property (\bfprim -> read (show bfprim) == (bfprim :: Bfprim))

isBfprimNonIOSpec :: Spec
isBfprimNonIOSpec =
  it "checks if it contains IO instructions" $ do
    isBfprimNonIO (read "+") `shouldBe` True
    isBfprimNonIO (read "-") `shouldBe` True
    isBfprimNonIO (read ">") `shouldBe` True
    isBfprimNonIO (read "<") `shouldBe` True
    isBfprimNonIO (read ".") `shouldBe` False
    isBfprimNonIO (read ",") `shouldBe` False
    isBfprimNonIO (read "[+++><]") `shouldBe` True
    isBfprimNonIO (read "[+++>.<]") `shouldBe` False
    isBfprimNonIO (read "[+++>[++++[+++><]<<<]]") `shouldBe` True
    isBfprimNonIO (read "[+++>[++++[+++,<]<<<]]") `shouldBe` False

isBfWhileSpec :: Spec
isBfWhileSpec =
  it "checks if it is BfWhile" $ do
    isBfWhile (read "+") `shouldBe` False
    isBfWhile (read ">") `shouldBe` False
    isBfWhile (read ".") `shouldBe` False
    isBfWhile (read ",") `shouldBe` False
    isBfWhile (read "[]") `shouldBe` True
    isBfWhile (read "[++++]") `shouldBe` True
    isBfWhile (read "[-]") `shouldBe` True
    isBfWhile (read "[+++>+++>+++<.<.]") `shouldBe` True
    isBfWhile (read "[+++>[[-]+]+>+++<.<.]") `shouldBe` True
