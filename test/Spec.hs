import Test.Hspec
import PN (pnSucc, baseOf, toDecimal)

main :: IO ()
main = hspec $ do
  describe "PN.toDecimal" $ do
    it "returns the decimal representation of the number" $ do
      toDecimal [1]         `shouldBe` 2
      toDecimal [1,0,0,0,0] `shouldBe` 11
      toDecimal [1,2,3]     `shouldBe` 360

  describe "PN.baseOf" $ do
    it "returns the correct base of the pn number" $ do
      baseOf [1]       `shouldBe` 1
      baseOf [2]       `shouldBe` 2
      baseOf [0,2]     `shouldBe` 2
      baseOf [0,0,2]   `shouldBe` 2
      baseOf [1,0]     `shouldBe` 2
      baseOf [0,1,3]   `shouldBe` 3
      baseOf [0,1,0]   `shouldBe` 2

  describe "PN.pnSucc" $ do
    it "returns the next pn number" $ do
      pnSucc [0]       `shouldBe` [1]
      pnSucc [1]       `shouldBe` [0,2]
      pnSucc [0,2]     `shouldBe` [1,0]
      pnSucc [1,0]     `shouldBe` [1,1]
      pnSucc [1,1]     `shouldBe` [1,2]
      pnSucc [1,2]     `shouldBe` [2,0]
      pnSucc [2,0]     `shouldBe` [2,1]
      pnSucc [2,1]     `shouldBe` [2,2]
      pnSucc [2,2]     `shouldBe` [0,0,3]
      pnSucc [0,0,3]   `shouldBe` [0,1,3]
      pnSucc [0,1,3]   `shouldBe` [0,2,3]
      pnSucc [0,2,3]   `shouldBe` [0,3,0]
      pnSucc [0,3,3]   `shouldBe` [1,0,0]
      pnSucc [3,2,3]   `shouldBe` [3,3,0]
      pnSucc [3,3,2]   `shouldBe` [3,3,3]
      pnSucc [3,3,3]   `shouldBe` [0,0,0,4]
      pnSucc [0,0,0,4] `shouldBe` [0,0,1,4]
      pnSucc [0,4,4,4] `shouldBe` [1,0,0,0]
      pnSucc [0,1,10]  `shouldBe` [0,0,0,0,0,0,0,0,2,10]

    it "accepts any number of leading zeros" $ do
      pnSucc [2]       `shouldBe` [1,0]
      pnSucc [0,2]     `shouldBe` [1,0]
      pnSucc [0,0,2]   `shouldBe` [1,0]
      pnSucc [3]       `shouldBe` [0,1,3]
      pnSucc [4]       `shouldBe` [0,0,1,4]
