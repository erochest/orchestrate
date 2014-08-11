{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Database.Orchestrate.Collection
    ( deleteCollection
    ) where


import           Network.Wreq
import qualified Data.Text as T

import           Database.Orchestrate.Network
import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils


deleteCollection :: Collection -> OrchestrateIO ()
deleteCollection c =
        api [c] ["force" := ("true" :: T.Text)] Nothing deleteWith
    >>= checkResponse
