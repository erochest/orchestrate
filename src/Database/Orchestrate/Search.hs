{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{- |
This module implements the <http://orchestrate.io/api/search Orchestrate Search API>.
-}

-- TODO: Create class for query values. Instantiate on Text and [(Text,
-- Text)], maybe also on ADTs for representing the query structures.

module Database.Orchestrate.Search
    (
    -- * Types
      QueryText

    -- ** List of Search Results
    , SearchList(..)
    , searchResults
    , searchTotal

    -- ** Single Search Result
    , SearchItem(..)
    , searchItem
    , searchScore

    -- * API Function
    , query
    ) where


import           Control.Applicative
import           Control.Error
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import qualified Data.Text                  as T
import           Network.Wreq

import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils


-- | The type for a query. This should be a Lucene query. See
-- the <http://orchestrate.io/api/search API documentation>.
type QueryText = T.Text

-- | A single search item.
data SearchItem v = SearchItem
                  { _searchItem  :: !(ResultItem Path v)
                  -- ^ The path to the item and the item itself.
                  , _searchScore :: !Double
                  -- ^ The item's relevancy to the query.
                  } deriving (Show)
$(makeLenses ''SearchItem)

instance FromJSON v => FromJSON (SearchItem v) where
    parseJSON o'@(Object o) =   SearchItem
                            <$> parseJSON o'
                            <*> o .: "score"
    parseJSON _             = mzero

-- | The collection of search results.
data SearchList v = SearchList
                  { _searchResults :: !(ResultList (SearchItem v))
                  -- ^ The list of search results.
                  , _searchTotal   :: !Int
                  -- ^ The total number of hits for the search. This may be
                  -- more than the number of results returned.
                  } deriving (Show)
$(makeLenses ''SearchList)

instance FromJSON v => FromJSON (SearchList v) where
    parseJSON o'@(Object o) =   SearchList
                            <$> parseJSON o'
                            <*> o .: "total_count"
    parseJSON _             = mzero

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
