{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}


module Specs.Orchestrate.RefSpec where


import           Control.Applicative
import           Control.Exception             (SomeException, bracket_)
import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Lazy          as BS
import           Data.Either
import           Data.Maybe

import           Test.Hspec

import           Database.Orchestrate.KeyValue
import           Database.Orchestrate.Ref
import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils

import           Specs.Orchestrate.Spec.Types
import           Specs.Orchestrate.Spec.Utils

runRefList :: OrchestrateIO (ResultList (TombstoneItem Person))
           -> IO (Either SomeException (ResultList (TombstoneItem Person)))
runRefList = run

cats :: [Person]
cats = map (uncurry Person) . (`zip` [1..]) . take 7 $ repeat "elsa"

withCats :: IO () -> IO ()
withCats = withFixtures cats

#if NETWORK_SPECS
spec :: Spec
spec = describe "Database.Orchestrate.Ref" $ do
    describe "getRef" $
        it "can return old version of objects." $ do
            r <- run $ putV (Person "eric" 42) NoMatch
            let ref = r ^? _Right . locationRef
            ref `shouldSatisfy` isn't _Nothing

            run' $ putV (Person "eric" 44) NoMatch
            eric <- run . getRef "test-coll" "eric" $ fromJust ref
            eric ^? _Right . _Just . personAge `shouldBe` Just 42

            run' $ purgeV (Person "eric" undefined) Nothing

    describe "decoding ResultList (TombstoneItem Person)." $ do
        it "should work." $ do
            r <- (eitherDecode <$> BS.readFile "specs/data.json") :: IO (Either String (ResultList (TombstoneItem Person)))
            r `shouldSatisfy` isRight

    describe "listRefs" $ around_ withCats $ do
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

#else
spec :: Spec
spec = describe "Database.Orchestrate.Ref" $ do
    it "should contain tests." $
        pendingWith "configure with \"--enable-tests -fnetwork-specs\"."
#endif
