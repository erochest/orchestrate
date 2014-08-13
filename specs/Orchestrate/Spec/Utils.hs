{-# LANGUAGE OverloadedStrings #-}


module Orchestrate.Spec.Utils where


import qualified Control.Exception             as Ex

import           Database.Orchestrate.KeyValue (getKV)
import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils

import           Orchestrate.Spec.Types


run :: OrchestrateIO m -> IO (Either Ex.SomeException m)
run m = envSession >>= runO' m

run' :: OrchestrateIO m -> IO ()
run' m = envSession >>= runO' m >> return ()

getPerson :: Key -> IO (Either Ex.SomeException (Maybe Person))
getPerson = run . getKV "test-coll"
