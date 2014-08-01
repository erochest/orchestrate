{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Database.Orchestrate.Collection
    ( deleteCollection
    ) where


import           Data.Monoid
import qualified Data.Text                  as T
import           Network.Wreq

import           Database.Orchestrate.Network
import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils


deleteCollection :: Collection -> OrchestrateIO ()
deleteCollection c = do
    base <- baseUrl
    opts <- authOptions
    r <-  io
        . deleteWith opts
        . T.unpack
        $  mconcat [base, "/", c, "?force=true"]
    checkResponse r
