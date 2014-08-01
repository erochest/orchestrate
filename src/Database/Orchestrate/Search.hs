{-# LANGUAGE OverloadedStrings #-}


-- TODO: Create class for query values. Instantiate on Text and [(Text,
-- Text)], maybe also on ADTs for representing the query structures.

module Database.Orchestrate.Search
    ( QueryText
    , SearchList(..)
    , SearchItem(..)

    , query
    ) where


import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.Text                  as T

import           Database.Orchestrate.Types


type QueryText = T.Text

query :: Collection -> QueryText -> Maybe Int -> Maybe Int
      -> OrchestrateT m [SearchList v]
query = undefined

data SearchList v = SearchList
                  { searchResults :: !(ResultList (SearchItem v))
                  , searchTotal   :: !Int
                  } deriving (Show)

instance FromJSON v => FromJSON (SearchList v) where
    parseJSON o'@(Object o) =   SearchList
                            <$> parseJSON o'
                            <*> o .: "total_count"
    parseJSON _             = mzero

data SearchItem v = SearchItem
                  { searchItem  :: !(ResultItem Path v)
                  , searchScore :: !Double
                  } deriving (Show)

instance FromJSON v => FromJSON (SearchItem v) where
    parseJSON o'@(Object o) =   SearchItem
                            <$> parseJSON o'
                            <*> o .: "score"
    parseJSON _             = mzero
