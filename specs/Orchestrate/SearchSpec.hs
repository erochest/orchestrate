{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}


module Orchestrate.SearchSpec where


import           Control.Exception (SomeException, bracket_)
import           Control.Lens
import qualified Data.List as L
import           Data.Monoid
import qualified Data.Text as T

import           Test.Hspec

import           Database.Orchestrate.KeyValue
import           Database.Orchestrate.Search
import           Database.Orchestrate.Types

import           Orchestrate.Spec.Types
import           Orchestrate.Spec.Utils


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

withFixtures :: IO () -> IO ()
withFixtures =
    bracket_ (run' $ mapM_ (`putKV` NoMatch) fixtures)
             (run' $ mapM_ (`purgeKV` Nothing) fixtures)

runSearch :: OrchestrateIO (SearchList Person)
          -> IO (Either SomeException (SearchList Person))
runSearch = run

allNames :: forall c. (T.Text -> Const (Endo [T.Text]) T.Text)
         -> Either c (SearchList Person)
         -> Const (Endo [T.Text]) (Either c (SearchList Person))
allNames = _Right . searchResults . resultList . traverse . searchItem . itemValue . personName

spec :: Spec
spec = describe "Database.Orchestrate.Search" $
    it "should contain tests." $
        pendingWith "commented out."

spec' :: Spec
spec' = describe "Database.Orchestrate.Search" $ around withFixtures $
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
            s ^.. allNames
                `shouldBe` ["Darth Maul"]
        it "should offset the results returned." $ do
            s <- runSearch $ query "test-coll" "darth" (Just 1) (Just 1)
            s ^?  _Right . searchTotal `shouldBe` Just 3
            s ^.. allNames
                `shouldBe` ["Darth Vader"]