import Test.Hspec
import PN (pnSucc, base)

main :: IO ()
main = hspec $ do
  describe "PN.base" $ do
    it "returns the correct base of the pn number" $ do
      base [1] `shouldBe` 1
      base [1,0] `shouldBe` 2
      base [0,1,3] `shouldBe` 3
      base [0,1,0] `shouldBe` 2

  describe "PN.pnSucc" $ do
    it "returns the next pn number" $ do
      pnSucc [0] `shouldBe` [1]
      pnSucc [1] `shouldBe` [0,2]
      pnSucc [0,2] `shouldBe` [1,0]
      pnSucc [1,0] `shouldBe` [1,1]
      pnSucc [1,1] `shouldBe` [1,2]
      pnSucc [1,2] `shouldBe` [2,0]
      pnSucc [2,0] `shouldBe` [2,1]
      pnSucc [2,1] `shouldBe` [2,2]
      pnSucc [2,2] `shouldBe` [0,0,3]
      pnSucc [0,0,3] `shouldBe` [0,1,3]
      pnSucc [0,1,3] `shouldBe` [0,2,3]
      pnSucc [0,2,3] `shouldBe` [0,3,0]
      pnSucc [0,3,3] `shouldBe` [1,0,0]
      pnSucc [3,2,3] `shouldBe` [3,3,0]
      pnSucc [3,3,2] `shouldBe` [3,3,3]
      pnSucc [3,3,3] `shouldBe` [0,0,0,4]

      -- pnSucc [1,0] `shouldBe` [1,1]
      -- pnSucc [1,1] `shouldBe` [0,2]
      -- pnSucc [0,2] `shouldBe` [1,2]
      -- pnSucc [1,2] `shouldBe` [2,0]
      -- pnSucc [2,0] `shouldBe` [2,1]
      -- pnSucc [2,1] `shouldBe` [2,2]
      -- pnSucc [2,2] `shouldBe` [3]

