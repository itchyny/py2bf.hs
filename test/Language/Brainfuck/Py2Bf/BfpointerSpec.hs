module Language.Brainfuck.Py2Bf.BfpointerSpec where
import Test.Hspec
import Language.Brainfuck.Py2Bf.Bf
import Language.Brainfuck.Py2Bf.Bfpointer

spec :: Spec
spec =
  describe "Bfpcode"
    runSpec

hello :: Bfpcode
hello = Bfpcode
      [ Addp 0 9
      , Whilep 0 [ Addp 1 8, Addp 2 11, Addp 3 5, Addp 0 (-1) ]
      , Putp 1
      , Addp 2 2, Putp 2, Addp 2 7, Putp 2, Putp 2
      , Addp 2 3, Putp 2, Addp 3 (-1), Putp 3
      , Addp 3 (-12), Putp 3, Addp 2 8, Putp 2
      , Addp 2 (-8), Putp 2, Addp 2 3, Putp 2
      , Addp 2 (-6), Putp 2, Addp 2 (-8), Putp 2
      , Addp 3 1, Putp 3
      ]

runSpec :: Spec
runSpec =
  it "runs Bfpcode correctly" $
    runString hello `shouldBe` "Hello, world!"
