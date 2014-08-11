{-# LANGUAGE OverloadedStrings #-}


module Orchestrate.KeyValueSpec where


import qualified Control.Exception as Ex
import           Control.Lens                  hiding ((.=))
import           Control.Monad
import           Data.Either
import qualified Data.List                     as L

import           Test.Hspec

import           Database.Orchestrate.KeyValue
import           Database.Orchestrate.Types

import           Orchestrate.Spec.Types
import           Orchestrate.Spec.Utils


-- TODO: Use a proxy to cache results for testing

getPerson :: Key -> IO (Either Ex.SomeException (Maybe Person))
getPerson = run . getKV "test-coll"

spec :: Spec
spec = describe "Database.Orchestrate.KeyValue" $ do
    describe "getKV" $ do
        it "should return Nothing if the key isn't there." $ do
            r <- getPerson "name"
            r `shouldSatisfy` isn't (_Right . _Just)
    describe "putKV" $ do
        it "should insert a value into the database." $ do
            r <- run $ putKV (Person "eric" 44) NoMatch
            r `shouldSatisfy` isRight
            r' <- getPerson "eric"
            r' ^? _Right . _Just `shouldBe` Just (Person "eric" 44)
    describe "postV" $ do
        it "should insert a value and get back a key." $ do
            let elsa = Person "elsa" 10
            Right (_, Just k) <- run (postV elsa)
            e <- getPerson k
            e ^? _Right . _Just `shouldBe` Just (Person "elsa" 10)
            void . run $ purgeV k (Person "elsa" undefined) Nothing
    describe "deleteKV" $ do
        it "should remove a value from the database." $ do
            r <- run $ deleteKV (Person "eric" undefined) Nothing
            r `shouldSatisfy` isRight
            r' <- getPerson "eric"
            r' `shouldSatisfy` isn't (_Right . _Just)
    describe "listKV" $ do
        it "should retrieve values from the database." $ do
            let names = ["abbie", "bob", "carol"]
            r <- run . mapM_ ((`putKV` NoMatch) . uncurry Person) $ zip names [1..]
            r `shouldSatisfy` isRight
            r' <- run $ listKV "test-coll" Nothing (Open, Open)
            r' `shouldSatisfy` isRight
            let Right kvl = r'
            _resultCount kvl `shouldBe` 3
            L.sort (map (name . _itemValue) (_resultList kvl)) `shouldBe` names
            void . run $ mapM_ ((`purgeKV` Nothing) . (`Person` undefined)) names
