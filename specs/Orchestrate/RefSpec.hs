{-# LANGUAGE OverloadedStrings #-}


module Orchestrate.RefSpec where


import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.Either
import           Control.Applicative
import           Control.Exception (bracket_, SomeException)
import           Control.Lens
import           Data.Maybe

import           Test.Hspec

import           Database.Orchestrate.KeyValue
import           Database.Orchestrate.Ref
import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils

import           Orchestrate.Spec.Types
import           Orchestrate.Spec.Utils

runRefList :: OrchestrateIO (ResultList (TombstoneItem Person))
           -> IO (Either SomeException (ResultList (TombstoneItem Person)))
runRefList = run

withCats :: IO () -> IO ()
withCats =
    bracket_ (mapM_ (run' . (`putKV` NoMatch)) cats)
             (run' $ purgeKV (Person "elsa" undefined) Nothing)
    where cats = map (uncurry Person) . (`zip` [(1::Int)..]) . take 7
               $ repeat "elsa"

spec :: Spec
spec = describe "Database.Orchestrate.Ref" $ do
    it "should contain tests." $
        pendingWith "commented out."

spec' :: Spec
spec' = do
    describe "getRef" $
        it "can return old version of objects." $ do
            r <- run $ putKV (Person "eric" 42) NoMatch
            let ref = r ^? _Right . locationRef
            ref `shouldSatisfy` isn't _Nothing

            run' $ putKV (Person "eric" 44) NoMatch
            eric <- run . getRef "test-coll" "eric" $ fromJust ref
            eric ^? _Right . _Just . personAge `shouldBe` Just 42

            run' $ purgeKV (Person "eric" undefined) Nothing

    describe "decoding ResultList (TombstoneItem Person)." $ do
        it "should work." $ do
            r <- (eitherDecode <$> BS.readFile "data.json") :: IO (Either String (ResultList (TombstoneItem Person)))
            r `shouldSatisfy` isRight

    describe "listRefs" $ around withCats $ do
        it "returns a list of references for an object." $ do
            refs <- runRefList $ listRefs "test-coll" "elsa" Nothing Nothing False
            refs ^? _Right . resultCount `shouldBe` Just 7
        it "returns a list of empty live values." $ do
            refs <- runRefList $ listRefs "test-coll" "elsa" Nothing Nothing False
            length (refs ^.. _Right . resultList . traverse . _LiveItem . liveValue . _Nothing)
                `shouldBe` 7
        it "returns a list of values if requested." $ do
            refs <- runRefList $ listRefs "test-coll" "elsa" Nothing Nothing True
            length (refs ^.. _Right . resultList . traverse . _LiveItem . liveValue . _Just)
                `shouldBe` 7
        it "limits the number of items requested." $ do
            refs <- runRefList $ listRefs "test-coll" "elsa" (Just 3) Nothing True
            refs ^.. _Right . resultList . to length `shouldBe` [3]
            refs ^.. _Right . resultList . traverse . _LiveItem . liveValue . _Just . personAge
                `shouldBe` [7, 6, 5]
        it "offsets the items returned." $ do
            refs <- runRefList $ listRefs "test-coll" "elsa" (Just 3) (Just 2) True
            refs ^.. _Right . resultList . to length `shouldBe` [3]
            refs ^.. _Right . resultList . traverse . _LiveItem . liveValue . _Just . personAge
                `shouldBe` [5, 4, 3]
