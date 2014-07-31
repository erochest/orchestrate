

module OrchestrateSpec where


import Test.Hspec


spec :: Spec
spec = describe "OrchestrateSpec" $ do
    it "should pass." $ ('a' == 'a') `shouldBe` True
    it "should fail." $ ('a' == 'b') `shouldBe` True

