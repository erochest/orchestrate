{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


-- TODO: Create class for query values. Instantiate on Text and [(Text,
-- Text)], maybe also on ADTs for representing the query structures.

module Database.Orchestrate.Search
    ( QueryText

    , SearchList(..)
    , searchResults
    , searchTotal

    , SearchItem(..)
    , searchItem
    , searchScore

    , query
    ) where


import           Control.Applicative
import           Control.Error
import qualified Control.Exception as Ex
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import qualified Data.Text                  as T
import           Network.Wreq

import           Database.Orchestrate.Network
import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils


type QueryText = T.Text

data SearchItem v = SearchItem
                  { _searchItem  :: !(ResultItem Path v)
                  , _searchScore :: !Double
                  } deriving (Show)
$(makeLenses ''SearchItem)

instance FromJSON v => FromJSON (SearchItem v) where
    parseJSON o'@(Object o) =   SearchItem
                            <$> parseJSON o'
                            <*> o .: "score"
    parseJSON _             = mzero

data SearchList v = SearchList
                  { _searchResults :: !(ResultList (SearchItem v))
                  , _searchTotal   :: !Int
                  } deriving (Show)
$(makeLenses ''SearchList)

instance FromJSON v => FromJSON (SearchList v) where
    parseJSON o'@(Object o) =   SearchList
                            <$> parseJSON o'
                            <*> o .: "total_count"
    parseJSON _             = mzero

query :: FromJSON v
      => Collection -> QueryText -> Maybe Int -> Maybe Int
      -> OrchestrateIO (SearchList v)
query c q limit offset = do
    s <- api [] [c] parms getWith
    checkResponse s
    orchestrateEither . fmapL errex . eitherDecode $ s ^. responseBody
    where errex = Ex.SomeException . Ex.ErrorCall
          parms = catMaybes [Just $ "query" := q
                            , ("limit"  :=) <$> limit
                            , ("offset" :=) <$> offset
                            ]
