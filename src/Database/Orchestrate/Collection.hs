{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Database.Orchestrate.Collection
    ( deleteCollection
    ) where


import qualified Data.Text                    as T
import           Network.Wreq

import           Database.Orchestrate.Network
import           Database.Orchestrate.Types


deleteCollection :: Collection -> OrchestrateIO ()
deleteCollection c =   ask
                   >>= restGet [] [c] ["force" := ("true" :: T.Text)]
                   >>= checkResponse
