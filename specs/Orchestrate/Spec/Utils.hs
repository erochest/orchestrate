module Orchestrate.Spec.Utils where


import qualified Control.Exception          as Ex

import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils


run :: OrchestrateIO m -> IO (Either Ex.SomeException m)
run m = envSession >>= runO' m

