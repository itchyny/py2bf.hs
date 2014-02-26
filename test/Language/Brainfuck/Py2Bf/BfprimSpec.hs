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
spec = do
  describe "Bfprim" $ do
    read_spec
    read_list_spec
    read_show_spec
    isBfprimIO_spec 
    isBfWhile_spec 

read_spec :: Spec
read_spec =
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

read_list_spec :: Spec
read_list_spec =
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

read_show_spec :: Spec
read_show_spec =
  it "read . show == id" $ do
    property (\bfprim -> read (show bfprim) == (bfprim :: Bfprim))

isBfprimIO_spec :: Spec
isBfprimIO_spec =
  it "checks if contains an IO" $ do
    isBfprimIO (read "+") `shouldBe` False
    isBfprimIO (read "-") `shouldBe` False
    isBfprimIO (read ">") `shouldBe` False
    isBfprimIO (read "<") `shouldBe` False
    isBfprimIO (read ".") `shouldBe` True
    isBfprimIO (read ",") `shouldBe` True
    isBfprimIO (read "[+++><]") `shouldBe` False
    isBfprimIO (read "[+++>.<]") `shouldBe` True
    isBfprimIO (read "[+++>[++++[+++,<]<<<]]") `shouldBe` True

isBfWhile_spec :: Spec
isBfWhile_spec =
  it "checks if it is BfWhile" $ do
    isBfWhile (read "+") `shouldBe` False
    isBfWhile (read ">") `shouldBe` False
    isBfWhile (read ".") `shouldBe` False
    isBfWhile (read ",") `shouldBe` False
    isBfWhile (read "[]") `shouldBe` True
    isBfWhile (read "[++++]") `shouldBe` True
    isBfWhile (read "[-]") `shouldBe` True
    isBfWhile (read "[+++>+++>+++<.<.]") `shouldBe` True
