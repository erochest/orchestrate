{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}


module Specs.Orchestrate.SearchSpec where


import           Control.Exception             (SomeException, bracket_)
import           Control.Lens
import qualified Data.List                     as L
import           Data.Monoid
import qualified Data.Text                     as T

import           Test.Hspec

import           Database.Orchestrate.KeyValue
import           Database.Orchestrate.Search
import           Database.Orchestrate.Types

import           Specs.Orchestrate.Spec.Types
import           Specs.Orchestrate.Spec.Utils


-- From http://www.empireonline.com/features/30-star-wars-characters
fixtures :: [Person]
fixtures = map (uncurry Person)
         $ (`zip` [(1::Int)..])
         [ "Han Solo"
         , "Darth Vader"
         , "Boba Fett"
         , "R2-D2"
         , "Chewbacca"
         , "Yoda"
         , "Luke Skywalker"
         , "Darth Maul"
         , "Stormtrooper"
         , "Princess Leia"
         , "Jabba the Hut"
         , "Ben Kenobi"
         , "Darth Sidious"
         , "Jawa"
         , "Lando Calrissian"
         , "Anakin Skywalker"
         , "Scout Trooper"
         , "Tuskan Raiders"
         , "Greedo"
         , "Tie Fighter Pilot"
         , "Obi-Wan Kenobi"
         , "Imperial Guards"
         , "Qui-Gin Jinn"
         , "C-3PO"
         , "Gamorrean Guards"
         , "Padme Amidala"
         , "Admiral Ackbar"
         , "Count Dooku"
         , "Mace Windu"
         , "Wicket"
         ]

runSearch :: OrchestrateIO (SearchList Person)
          -> IO (Either SomeException (SearchList Person))
runSearch = run

allNames :: forall c. (T.Text -> Const (Endo [T.Text]) T.Text)
         -> Either c (SearchList Person)
         -> Const (Endo [T.Text]) (Either c (SearchList Person))
allNames = _Right . searchResults . resultList . traverse . searchItem . itemValue . personName

#if NETWORK_SPECS
spec :: Spec
spec = describe "Database.Orchestrate.Search" $ around_ (withFixtures fixtures) $
    describe "query" $ do
        it "should search for all fields." $ do
            s <- runSearch $ query "test-coll" "darth" Nothing Nothing
            s ^?  _Right . searchTotal `shouldBe` Just 3
            L.sort (s ^.. allNames)
                `shouldBe` ["Darth Maul", "Darth Sidious", "Darth Vader"]
        it "should search in a specific field." $ do
            s <- runSearch $ query "test-coll" "name=guards" Nothing Nothing
            s ^?  _Right . searchTotal `shouldBe` Just 2
            L.sort (s ^.. allNames)
                `shouldBe` ["Gamorrean Guards", "Imperial Guards"]
        it "should limit the number of results returned." $ do
            s <- runSearch $ query "test-coll" "darth" (Just 1) Nothing
            s ^?  _Right . searchTotal `shouldBe` Just 3
            s ^?  _Right . searchResults . resultCount `shouldBe` Just 1
        it "should offset the results returned." $ do
            s <- runSearch $ query "test-coll" "darth" Nothing (Just 2)
            s ^?  _Right . searchTotal `shouldBe` Just 3
            s ^?  _Right . searchResults . resultCount `shouldBe` Just 1

#else
spec :: Spec
spec = describe "Database.Orchestrate.Search" $
    it "should contain tests." $
        pendingWith "configure with \"--enable-tests -fnetwork-specs\"."
#endif
