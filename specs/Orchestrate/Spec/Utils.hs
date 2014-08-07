module Orchestrate.Spec.Utils where


import qualified Data.Text                     as T

import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils


run :: OrchestrateIO m -> IO (Either T.Text m)
run m = envSession >>= runO' m

