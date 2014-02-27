module Language.Brainfuck.Py2Bf.BfSpec where
import Test.Hspec
import Test.QuickCheck
import Language.Brainfuck.Py2Bf.Bf
import Language.Brainfuck.Py2Bf.Bfprim

spec :: Spec
spec = do
  describe "Bf" $ do
    show_spec
    read_spec
    skip_spec
    show_read_spec

show_spec :: Spec
show_spec =
  it "shows Bfcode correctly" $ do
    property (\n -> let m = n `mod` 1000 in
                        filter (/='\n') (show (Bfcode (replicate m BfIncr ++ [BfPut])))
                                                    == replicate m '+' ++ ".")

read_spec :: Spec
read_spec =
  it "reads Bfcode correctly" $ do
    read "+++" `shouldBe` Bfcode [BfIncr, BfIncr, BfIncr]
    read "---" `shouldBe` Bfcode [BfDecr, BfDecr, BfDecr]
    read ">>>" `shouldBe` Bfcode [BfNext, BfNext, BfNext]
    read "<<<" `shouldBe` Bfcode [BfPrev, BfPrev, BfPrev]
    read "..." `shouldBe` Bfcode [BfPut, BfPut, BfPut]
    read ",,," `shouldBe` Bfcode [BfGet, BfGet, BfGet]
    read "[][][]" `shouldBe` Bfcode [BfWhile [], BfWhile [], BfWhile []]
    read "[+++--->>><<<]" `shouldBe`
      Bfcode [BfWhile
        [BfIncr, BfIncr, BfIncr, BfDecr, BfDecr, BfDecr,
         BfNext, BfNext, BfNext, BfPrev, BfPrev, BfPrev]]

skip_spec :: Spec
skip_spec =
  it "skips whites" $ do
    read "   +   +   +   " `shouldBe`
      Bfcode [BfIncr, BfIncr, BfIncr]
    read "\n\n[\t\t]  [\n\n]\t\t[  ]\n\n" `shouldBe`
      Bfcode [BfWhile [], BfWhile [], BfWhile []]
    read "  [\t\t+\n+\n+\n--\n->\t>>\t<<\t<]" `shouldBe`
      Bfcode [BfWhile
        [BfIncr, BfIncr, BfIncr, BfDecr, BfDecr, BfDecr,
         BfNext, BfNext, BfNext, BfPrev, BfPrev, BfPrev]]

show_read_spec :: Spec
show_read_spec =
  it "show . read" $ do
    show (read "" :: Bfcode) `shouldBe` ""
    show (read "[]" :: Bfcode) `shouldBe` ""
    show (read "+++" :: Bfcode) `shouldBe` ""
    show (read "+++[+++]" :: Bfcode) `shouldBe` ""
    show (read "+++[>+++<-]>" :: Bfcode) `shouldBe` ""
    show (read "+++[>+++<-]>." :: Bfcode) `shouldBe` "+++[>+++<-]>."
    show (read "+++[>+++<-][>>][<<][++]>." :: Bfcode) `shouldBe` "+++[>+++<-]>."
