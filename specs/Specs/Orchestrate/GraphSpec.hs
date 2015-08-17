{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}


module Specs.Orchestrate.GraphSpec where


import           Control.Error
import           Control.Exception
import           Control.Lens
import           Control.Monad

import           Test.Hspec

import           Database.Orchestrate.Graph
import           Database.Orchestrate.KeyValue
import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils

import           Specs.Orchestrate.Spec.Types
import           Specs.Orchestrate.Spec.Utils


-- Names for 2014 tropical cyclones (from NOAA).
fixtures :: [Person]
fixtures = [ Person "Bertha"    1    -- 0
           , Person "Fay"       9    -- 1
           , Person "Laura"     8    -- 2
           , Person "Rene"      2    -- 3
           ]


#if NETWORK_SPECS
spec :: Spec
spec = describe "Database.Orchestrate.Graph" $ around_ (withFixtures fixtures) $ do
    describe "createRel and getRel" $
        it "should create relationships that getRel can retrieve." $ do
            let bertha = fixtures !! 0
                rene   = fixtures !! 3

            r' <- run $ createRel bertha "brother" rene
            r' `shouldSatisfy` isRight

            r <- run $ getRel bertha "brother" []
            r ^? _Right . resultCount `shouldBe` Just 1
            r ^.. _Right . resultList . traverse . itemValue . personName
                `shouldBe` ["Rene"]

    describe "deleteRel" $
        it "should have tests" $ do
            let fay   = fixtures !! 1
                laura = fixtures !! 2
            void . run' $ createRel fay "sister" laura
            void . run' $ deleteRel fay "sister" laura
            r <- (run $ getRel fay "sister" []) :: IO (Either SomeException (RelList Person Person))
            r ^? _Right . resultCount `shouldBe` Just 0

#else
spec :: Spec
spec = describe "Database.Orchestrate.Graph" $
    it "should contain tests." $
        pendingWith "configure with \"--enable-tests -fnetwork-specs\"."
#endif
