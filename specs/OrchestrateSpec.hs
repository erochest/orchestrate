{-# LANGUAGE OverloadedStrings #-}


module OrchestrateSpec where


import           Control.Lens

import           Test.Hspec

import           Database.Orchestrate.Utils


spec :: Spec
spec = describe "Database.Orchestrate.Utils" $ do
    describe "locationCollection" $ do
        it "should extract the collection from a location URL." $ do
            shouldBe ("/v0/collection/035ab997adffe604/refs/82eafab14dc84ed3" ^? locationCollection)
                     (Just "collection")
            shouldBe ("/v0/collection/key/events/type/1398286518286/6" ^? locationCollection)
                     (Just "collection")
        it "should return Nothing if the URL is invalid." $ do
            "/v0" ^? locationCollection `shouldBe` Nothing

    describe "locationKey" $ do
        it "should extract the key from a location URL." $ do
            shouldBe ("/v0/collection/035ab997adffe604/refs/82eafab14dc84ed3" ^? locationKey)
                     (Just "035ab997adffe604")
            shouldBe ("/v0/collection/key/events/type/1398286518286/6" ^? locationKey)
                     (Just "key")
        it "should return Nothing if the URL is too short." $ do
            "/v0/collection" ^? locationKey `shouldBe` Nothing

    describe "locationRef" $ do
        it "should extract the ref from a location URL." $ do
            shouldBe ("/v0/collection/035ab997adffe604/refs/82eafab14dc84ed3" ^? locationRef)
                     (Just "82eafab14dc84ed3")
        it "should return Nothing if the URL is too short." $ do
            "/v0/collection" ^? locationRef `shouldBe` Nothing

    describe "locationType" $ do
        it "should extract the type of event from the location URL." $ do
            shouldBe ("/v0/collection/key/events/type/1398286518286/6" ^? locationType)
                     (Just "type")
        it "should return Nothing if the URL is too short." $ do
            "/v0/collection/key/events" ^? locationType `shouldBe` Nothing

    describe "locationTimestamp" $ do
        it "should extract the event timesteamp from the location URL." $ do
            shouldBe ("/v0/collection/key/events/type/1398286518286/6" ^? locationTimestamp)
                     (Just 1398286518286)
        it "should return Nothing if the URL is too short." $ do
            "/v0/collection/key/events" ^? locationTimestamp `shouldBe` Nothing

    describe "locationOrdinal" $ do
        it "should extract the event ordinal from the location URL." $ do
            shouldBe ("/v0/collection/key/events/type/1398286518286/6" ^? locationOrdinal)
                     (Just 6)
        it "should return Nothing if the URL is too short." $ do
            "/v0/collection/key/events" ^? locationOrdinal `shouldBe` Nothing
