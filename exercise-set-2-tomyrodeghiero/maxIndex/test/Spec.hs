import Test.Hspec


import qualified Data.List

import MaxIndex

main :: IO ()
main = hspec spec

 
spec :: Spec
spec = do

    describe "maxindexDec" $ do

        it "Test maxindexDec with descending order " $ do
                maxindexDec [5,4,3,2,1] `shouldBe` ((0) :: (Int))
        it "Test maxindexDec ascending order" $ do
                maxindexDec [1,2,3,4,5]  `shouldBe` ((4) :: (Int))  
        it "Test maxindexDec with random order" $ do
                maxindexDec [2,3,1,4]  `shouldBe` ((3) :: (Int))

    describe "maxindexFB" $ do

        it "Test maxindexFB with descending order " $ do
                maxindexFB [5,4,3,2,1] `shouldBe` ((0) :: (Int))
        it "Test maxindexFB ascending order" $ do
                maxindexFB [1,2,3,4,5]  `shouldBe` ((4) :: (Int))  
        it "Test maxindexFb with random order" $ do
                maxindexFB [2,3,1,4]  `shouldBe` ((3) :: (Int))

   