{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}


module Database.Orchestrate.Types
    ( APIKey
    , Collection
    , Key
    , Ref
    , Timestamp
    , Location
    , IfMatch
    , IfMatch'(..)
    , Range
    , RangeEnd
    , Limit
    , Offset

    , Session(..)
    , OrchestrateData(..)
    , OrchestrateT(..)
    , OrchestrateIO
    , Orchestrate

    , ResultList(..)
    , ResultItem(..)
    , Path(..)
    ) where


import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Text              as T


type APIKey     = T.Text
type Collection = T.Text
type Key        = T.Text
type Ref        = T.Text
type Timestamp  = Integer
type Location   = T.Text
type IfMatch    = Maybe Ref
type Limit      = Int
type Offset     = Int

data IfMatch'   = IfMatch Ref
                | IfNoneMatch Ref
                | NoMatch
                deriving (Show)

type Range a    = (RangeEnd a, RangeEnd a)
data RangeEnd a = Inclusive a
                | Exclusive a
                | Open
                deriving (Show, Functor)

data Session = Session
             { sessionKey     :: !APIKey
             , sessionVersion :: !Int
             } deriving (Show)

class (ToJSON a, FromJSON a) => OrchestrateData a where
    tableName :: a -> T.Text
    dataKey   :: a -> T.Text

newtype OrchestrateT m a
    = OrchestrateT { runOrchestrate :: EitherT T.Text (ReaderT Session m) a }
    deriving (Functor, Applicative, Monad)

type Orchestrate   = OrchestrateT Identity
type OrchestrateIO = OrchestrateT IO

data Path = Path
          { itemCollection :: !T.Text
          , itemKey        :: !T.Text
          , itemRef        :: !T.Text
          } deriving (Show)

instance FromJSON Path where
    parseJSON (Object o) =   Path
                         <$> o .: "collection"
                         <*> o .: "key"
                         <*> o .: "ref"
    parseJSON _          =   mzero

data ResultList i = ResultList
                  { resultCount :: !Int
                  , resultList  :: ![i]
                  , resultPrev  :: !(Maybe Location)
                  , resultNext  :: !(Maybe Location)
                  } deriving (Show)

instance FromJSON r => FromJSON (ResultList r) where
    parseJSON (Object o) =   ResultList
                         <$> o .: "count"
                         <*> o .: "results"
                         <*> o .: "prev"
                         <*> o .: "next"
    parseJSON _          =   mzero

data ResultItem p v = ResultItem
                    { resultItemPath  :: !p
                    , resultItemValue :: !v
                    } deriving (Show)

instance (FromJSON p, FromJSON v) => FromJSON (ResultItem p v) where
    parseJSON (Object o) =   ResultItem
                         <$> o .: "path"
                         <*> o .: "value"
    parseJSON _          =   mzero
