{-# LANGUAGE OverloadedStrings #-}

{-|
This module implements the <http://orchestrate.io/api/graph Graph API>.
-}

module Database.Orchestrate.Graph
    (
    -- * Types
      RelKind
    , RelList

    -- * API Functions
    , getRel
    , createRel
    , deleteRel
    ) where


import           Control.Monad
import           Data.Aeson
import qualified Data.Text                  as T
import           Network.Wreq

import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils


-- | This is the kind of relationship that the graph edge represents.
type RelKind     = T.Text

-- | A list of edges returned by 'getRel'.
--
-- This datatype uses two parameters:
--
-- [@a@] The data type for the edge's origin node.
-- [@b@] The data type for the edge's target node.
type RelList a b = ResultList (ResultItem Path b)

-- | This retrieves a list of target nodes from edges originating from @a@.
-- For more information see <http://orchestrate.io/api/graph the API documentation>.
--
-- If the third parameter is not empty, it represents additional edges that
-- will be traversed to get to the target.
--
-- For example:
--
-- > getRel data "friend" ["last_name"]
getRel :: (OrchestrateData a, FromJSON b)
       => a                             -- ^ The originating node.
       -> RelKind                       -- ^ The first edge to traverse.
       -> [RelKind]                     -- ^ Any additional edges to traverse.
       -> OrchestrateIO (RelList a b)   -- ^ A list of nodes originating from @a@.
getRel from rel rels = apiCheckDecode [] url [] getWith
    where url = tableName from : dataKey from : "relations" : rel : rels

-- | Creates a relationship (an edge) between two nodes. The edge has
-- a 'RelKind' type. See the <http://orchestrate.io/api/graph API documentation>
-- for more information.
--
-- For example:
--
-- > createRel start "parent" child
createRel :: (OrchestrateData a, OrchestrateData b)
          => a                  -- ^ The originating node.
          -> RelKind            -- ^ The label for the edge.
          -> b                  -- ^ The target, destination node.
          -> OrchestrateIO ()
createRel from rel to = void $ apiCheck [] url [] $ \o s -> putWith o s Null
    where url = [ tableName from , dataKey from
                , "relation", rel
                , tableName to, dataKey to
                ]

-- | This removes a relationship (an edge) between two nodes.
--
-- For example:
--
-- > deleteRel start "parent" child
deleteRel :: (OrchestrateData a, OrchestrateData b)
          => a                  -- ^ The originating node.
          -> RelKind            -- ^ The label for the edge.
          -> b                  -- ^ The target, destination node.
          -> OrchestrateIO ()
deleteRel from rel to =
    void $ apiCheck [] url ["purge" := ("true" :: T.Text)] deleteWith
    where url = [ tableName from, dataKey from
                , "relation", rel
                , tableName to, dataKey to
                ]
