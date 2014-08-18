{-# LANGUAGE CPP               #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}


module Specs.Orchestrate.EventSpec where


import           Control.Applicative
import           Control.Error
import           Control.Exception
import           Control.Lens
import           Control.Monad
import qualified Data.Text                     as T
import           Network.Wreq

import           Test.Hspec

import           Database.Orchestrate.Events
import           Database.Orchestrate.KeyValue
import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils

import           Specs.Orchestrate.Spec.Types
import           Specs.Orchestrate.Spec.Utils


type GetEvent = IO (Either SomeException (Maybe (EventItem Event Person)))

fixtures :: [Person]
fixtures = [ Person "joe" 42
           , Person "ella" 13
           ]

events :: [(Event, Timestamp)]
events = [ (Event "birth"    0.0, 784111777000)
         , (Event "marriage" 0.5, 784111777100)
         , (Event "child"    0.6, 784111777200)
         , (Event "divorce"  0.8, 784111777300)
         , (Event "death"    1.0, 784111777400)
         ]

deleteLocs :: [Location] -> IO ()
deleteLocs locs = do
    Session{_sessionOptions,_sessionURL} <- envSession
    let url = T.unpack _sessionURL
    mapM_ (deleteWith _sessionOptions . (++ "?purge=true") . (url ++) . T.unpack) locs

withEvents :: Person -> EventType -> [(Event, Timestamp)] -> IO () -> IO ()
withEvents p et evs = withEvents' p et evs . const

withEvents' :: Person -> EventType -> [(Event, Timestamp)]
            -> (Either SomeException [Location] -> IO ())
            -> IO ()
withEvents' p et evs action =
    bracket (run . mapM (uncurry (createEvent p et)) $ map (fmap Just) evs)
            (either (const $ return ()) deleteLocs)
            action

callLoc :: Location -> (Timestamp -> Int -> OrchestrateIO a)
        -> IO (Either SomeException a)
callLoc loc f = run . join $ f <$> eithererr loc (loc ^? locationTimestamp)
                               <*> eithererr loc (loc ^? locationOrdinal)

eithererr :: Monad m => Location -> Maybe a -> OrchestrateT m a
eithererr loc = orchestrateEither
              . note (SomeException
                        (ErrorCall $ "Invalid location: " ++ T.unpack loc))

#if NETWORK_SPECS
spec :: Spec
spec = describe "Database.Orchestrate.Events" $ around (withFixtures fixtures) $ do
    describe "createEvent" $
        it "should create events." $
            withEvents' (fixtures !! 1) "create" events $ \locs -> do
                length (locs ^.. _Right . traverse) `shouldBe` 5
                locs ^.. _Right . traverse . locationKey  `shouldBe` replicate 5 "ella"
                locs ^.. _Right . traverse . locationType `shouldBe` replicate 5 "create"
                length (locs ^.. _Right . traverse . locationTimestamp) `shouldBe` 5
                length (locs ^.. _Right . traverse . locationOrdinal)   `shouldBe` 5

    describe "getEvent" $
        it "should retrieve the stored data for an event." $
            let p = head fixtures
            in  withEvents' p "get" events $ \(Right locs) -> do
                let events' = map fst events
                    items   = zip events'
                            $ zipWith locationEventItem locs events'
                forM_ items $ \(event, Just evtItem) -> do
                    e <- run $ getEvent p "get" (evtItem ^. eventTime) (evtItem ^. eventOrd)
                    e ^? _Right . _Just . eventItem . itemValue `shouldBe` Just event

    describe "updateEvent" $
        it "should update the data in the event." $
            let ev@(e, _) = head events
                p         = fixtures !! 1
            in  withEvents' p "life" [ev] $ \(Right [loc]) -> do
                let e' = e & eventScale .~ 3.1415
                r <- callLoc loc $ \t o -> updateEvent p "life" e' t o Nothing
                r `shouldSatisfy` isRight
                mNewE <- callLoc loc $ getEvent p "life"
                mNewE ^? _Right . _Just . eventItem . itemValue . eventScale
                    `shouldBe` Just 3.1415

    describe "deleteEvent" $
        it "should remove the event" $ do
            let (e, ts) = head events
                p       = fixtures !! 1
            Right loc <- run $ createEvent p "test" e (Just ts)
            r <- callLoc loc $ \t o -> deleteEvent p "test" t o Nothing
            r `shouldSatisfy` isRight
            Right e' <- (run $   eithererr loc (loc ^? locationOrdinal)
                             >>= getEvent p "test" ts) :: GetEvent
            e' `shouldSatisfy` isNothing

    describe "listEvents" $ around (withEvents (head fixtures) "list" events) $ do
        it "should return all events" $ do
            elist <- run $ listEvents (head fixtures) "list" Nothing (Open, Open)
            elist ^? _Right . resultCount `shouldBe` Just (length events)
            elist ^.. _Right . resultList . traverse . eventItem . itemValue . eventTitle
                `shouldMatchList` ["birth", "marriage", "child", "divorce", "death"]

#else
spec :: Spec
spec = describe "Database.Orchestrate.Events" $
    it "should contain tests." $
        pendingWith "configure with \"--enable-tests -fnetwork-specs\"."
#endif
