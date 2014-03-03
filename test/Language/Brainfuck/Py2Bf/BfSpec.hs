module Language.Brainfuck.Py2Bf.BfSpec where
import Test.Hspec
import Test.QuickCheck
import Language.Brainfuck.Py2Bf.Bf
import Language.Brainfuck.Py2Bf.Bfprim

spec :: Spec
spec =
  describe "Bf" $ do
    showSpec
    readSpec
    skipSpec
    showReadSpec
    runSpec

showSpec :: Spec
showSpec =
  it "shows Bfcode correctly" $
    property (\n -> let m = n `mod` 1000 in
                        filter (/='\n') (show (Bfcode (replicate m BfIncr ++ [BfPut])))
                                                    == replicate m '+' ++ ".")

readSpec :: Spec
readSpec =
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

skipSpec :: Spec
skipSpec =
  it "skips whites" $ do
    read "   +   +   +   " `shouldBe`
      Bfcode [BfIncr, BfIncr, BfIncr]
    read "\n\n[\t\t]  [\n\n]\t\t[  ]\n\n" `shouldBe`
      Bfcode [BfWhile [], BfWhile [], BfWhile []]
    read "  [\t\t+\n+\n+\n--\n->\t>>\t<<\t<]" `shouldBe`
      Bfcode [BfWhile
        [BfIncr, BfIncr, BfIncr, BfDecr, BfDecr, BfDecr,
         BfNext, BfNext, BfNext, BfPrev, BfPrev, BfPrev]]

showReadSpec :: Spec
showReadSpec =
  it "show . read" $ do
    show (read "" :: Bfcode) `shouldBe` ""
    show (read "[]" :: Bfcode) `shouldBe` ""
    show (read "+++" :: Bfcode) `shouldBe` ""
    show (read "+++[+++]" :: Bfcode) `shouldBe` ""
    show (read "+++[>+++<-]>" :: Bfcode) `shouldBe` ""
    show (read "+++[>+++<-]>." :: Bfcode) `shouldBe` "+++[>+++<-]>."
    show (read "+++[>+++<-][>>][<<][++]>." :: Bfcode) `shouldBe` "+++[>+++<-]>."

hello :: String
hello = "+++++++++[->++++++++>+++++++++++>+++++<<<]>.>++.+++++++..+++.>-."
     ++ "------------.<++++++++.--------.+++.------.--------.>+."

runSpec :: Spec
runSpec =
  it "runs correctly" $
    runString (read hello :: Bfcode) `shouldBe` "Hello, world!"
