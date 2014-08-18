{-# LANGUAGE OverloadedStrings #-}


module Specs.Orchestrate.Spec.Utils where


import qualified Control.Exception             as Ex

import           Database.Orchestrate.KeyValue (getKV, purgeKV, putKV)
import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils

import           Specs.Orchestrate.Spec.Types


run :: OrchestrateIO m -> IO (Either Ex.SomeException m)
run m = envSession >>= runO' m

run' :: OrchestrateIO m -> IO ()
run' m = envSession >>= runO' m >> return ()

getPerson :: Key -> IO (Either Ex.SomeException (Maybe Person))
getPerson = run . getKV "test-coll"

withFixtures :: OrchestrateData a => [a] -> IO () -> IO ()
withFixtures fixtures =
    Ex.bracket_ (run' $ mapM_ (`putKV` NoMatch) fixtures)
                (run' $ mapM_ (`purgeKV` Nothing) fixtures)
