{-# LANGUAGE OverloadedStrings #-}


module Database.Orchestrate.Collection
    ( deleteCollection
    ) where


import           Control.Monad
import qualified Data.Text                  as T
import           Network.Wreq

import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils


deleteCollection :: Collection -> OrchestrateIO ()
deleteCollection c =
    void $ apiCheck [] [c] ["force" := ("true" :: T.Text)] deleteWith
