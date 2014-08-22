{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{- |
This module implements the <http://orchestrate.io/api/search Orchestrate Search API>.
-}

-- TODO: Create class for query values. Instantiate on Text and [(Text,
-- Text)], maybe also on ADTs for representing the query structures.

module Database.Orchestrate.Search
    ( query
    ) where


import           Control.Applicative
import           Control.Error
import           Data.Aeson
import           Network.Wreq

import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils


-- | This performs the query and returns the results.
--
-- > query "coll-name" "query" (Just 25) Nothing
query :: FromJSON v
      => Collection                     -- ^ The collection to query.
      -> QueryText                      -- ^ The query.
      -> Maybe Int                      -- ^ The maximum number of items to return.
      -> Maybe Int                      -- ^ The offset into the search results.
      -> OrchestrateIO (SearchList v)   -- ^ Returns the results of the query with their scores.
query c q limit offset = apiCheckDecode [] [c] parms getWith
    where parms = catMaybes [Just $ "query" := q
                            , ("limit"  :=) <$> limit
                            , ("offset" :=) <$> offset
                            ]
