{-# LANGUAGE OverloadedStrings #-}


module Specs.Orchestrate.Spec.Utils where


import qualified Control.Exception             as Ex
import           Prelude                       hiding (lookup)

import           Database.Orchestrate.KeyValue (lookup, purgeV, putV)
import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils

import           Specs.Orchestrate.Spec.Types


run :: OrchestrateIO m -> IO (Either Ex.SomeException m)
run m = envSession >>= runO' m

run' :: OrchestrateIO m -> IO ()
run' m = envSession >>= runO' m >> return ()

getPerson :: Key -> IO (Either Ex.SomeException (Maybe Person))
getPerson = run . lookup "test-coll"

withFixtures :: OrchestrateData a => [a] -> IO () -> IO ()
withFixtures fixtures =
    Ex.bracket_ (run' $ mapM_ (`putV` NoMatch) fixtures)
                (run' $ mapM_ (`purgeV` Nothing) fixtures)
