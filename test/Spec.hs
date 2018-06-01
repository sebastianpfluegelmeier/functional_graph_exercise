import Test.Hspec
import Control.Exception (evaluate)
import Matrix

main :: IO ()
main = hspec $ do
  let m1 = matrix 10 10 (\(x, y) -> x * y)
  let m2 = matrix 10 10 (\(x, y) -> x - y)
  describe "Matrix.getLeme" $ do
    it "returns an element from a matrix" $ do
      getElem 3 3 m1 `shouldBe` 9

  describe "Matrix.identity" $ do
    let mi = identity 8
    it "has ones in the diagonal" $ do
      getElem 1 1 mi `shouldBe` 1
    it "has zeoreos everywhere else" $ do
      getElem 2 4 mi `shouldBe` 0

  describe "Matrix.setElem" $ do
    let m1Updated = setElem 9 (2, 2) m1
    it "updates a entry in a Matrix" $ do
      getElem 2 2 m1Updated `shouldBe` 9

  describe "Matrix.elementwise" $ do
    it "performs a operation on two matrices elementwise" $ do
      foldr1 (+) m1 + foldr1 (+) m2 `shouldBe` (foldr1 (+) $ elementwise (+) m1 m2)


