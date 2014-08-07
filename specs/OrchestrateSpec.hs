{-# LANGUAGE OverloadedStrings #-}


module OrchestrateSpec where


import           Control.Lens

import           Test.Hspec

import           Database.Orchestrate.Utils


spec :: Spec
spec = describe "Database.Orchestrate.Utils" $ do
    describe "locationKey" $ do
        it "should extract the key from a location URL." $ do
            shouldBe ("/v0/collection/035ab997adffe604/refs/82eafab14dc84ed3" ^? locationKey)
                     (Just "035ab997adffe604")
        it "should return Nothing if the URL is too short." $ do
            "/v0/collection" ^? locationKey `shouldBe` Nothing

    describe "locationRef" $ do
        it "should extract the ref from a location URL." $ do
            shouldBe ("/v0/collection/035ab997adffe604/refs/82eafab14dc84ed3" ^? locationRef)
                     (Just "82eafab14dc84ed3")
        it "should return Nothing if the URL is too short." $ do
            "/v0/collection" ^? locationRef `shouldBe` Nothing
