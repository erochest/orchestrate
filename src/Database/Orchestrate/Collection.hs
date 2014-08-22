{-# LANGUAGE OverloadedStrings #-}


{-|
This implements the <http://orchestrate.io/api/collections API calls> to
manage collections on Orchestrate.
-}

module Database.Orchestrate.Collection
    ( deleteCollection
    ) where


import           Control.Monad
import qualified Data.Text                  as T
import           Network.Wreq

import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils


-- | This deletes a collection. See
-- <http://orchestrate.io/api/collections#delete the API documentation> for
-- more information.
--
-- > deleteCollection "collection-name"

deleteCollection :: Collection -> OrchestrateIO ()
deleteCollection c =
    void $ apiCheck [] [c] ["force" := ("true" :: T.Text)] deleteWith
