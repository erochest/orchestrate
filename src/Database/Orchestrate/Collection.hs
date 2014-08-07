{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Database.Orchestrate.Collection
    ( deleteCollection
    ) where


import           Network.Wreq

import           Database.Orchestrate.Network
import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils


deleteCollection :: Collection -> OrchestrateIO ()
deleteCollection c = checkResponse =<< api [c] ["force=true"] Nothing deleteWith
