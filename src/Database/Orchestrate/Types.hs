{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -Wall #-}


-- TODO: Use types from wreq for params, headers, etc.
-- TODO: Clean this up some. Eeek.

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
    , RangeEnd(..)
    , Limit
    , Offset
    , RestCall

    , Session(..)
    , sessionURL
    , sessionKey
    , sessionVersion
    , sessionOptions

    , OrchestrateData(..)
    , OrchestrateT(..)
    , OrchestrateIO
    , Orchestrate
    , orchestrateEither

    , ask
    , asks

    , io

    , throwError
    , catchError

    , ResultList(..)
    , resultCount
    , resultList
    , resultPrev
    , resultNext

    , ResultItem(..)
    , itemPath
    , itemValue

    , Path(..)
    , itemCollection
    , itemKey
    , itemRef
    ) where


import           Control.Applicative
import           Control.Error
import qualified Control.Exception         as Ex
import           Control.Lens
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Default
import qualified Data.Text                 as T
import           Network.Wreq


type APIKey     = T.Text
type Collection = T.Text
type Key        = T.Text
type Ref        = T.Text
type Timestamp  = Integer
type Location   = T.Text
type IfMatch    = Maybe Ref
type Limit      = Int
type Offset     = Int
type RestCall a = Options -> String -> IO (Response a)

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
             { _sessionURL     :: !T.Text
             , _sessionKey     :: !APIKey
             , _sessionVersion :: !Int
             , _sessionOptions :: !Options
             } deriving (Show)
$(makeLenses ''Session)

instance Default Session where
    def = Session "https://api.orchestrate.io" "" 0
        $ defaults & header "Content-Type" .~ ["application/json"]
                   & header "Accept"       .~ ["application/json"]

class (ToJSON a, FromJSON a) => OrchestrateData a where
    tableName :: a -> T.Text
    dataKey   :: a -> T.Text

newtype OrchestrateT m a
    = OrchestrateT
    { runOrchestrate :: EitherT Ex.SomeException (ReaderT Session m) a }
    deriving (Functor, Applicative, Monad)

instance MonadTrans OrchestrateT where
    lift = OrchestrateT . lift . lift

instance MonadIO m => MonadIO (OrchestrateT m) where
    liftIO = OrchestrateT . liftIO . liftIO

instance Monad m => MonadReader Session (OrchestrateT m) where
    ask     = OrchestrateT . lift $ ask
    local f = OrchestrateT . local f . runOrchestrate

io :: MonadIO m => IO a -> OrchestrateT m a
io = OrchestrateT . syncIO

-- TODO: Need to define this for other monad classes.

instance Monad m => MonadError Ex.SomeException (OrchestrateT m) where
    throwError = OrchestrateT . EitherT . return . Left
    catchError a handler =   join
                         .   fmap (handler' handler)
                         .   lift
                         .   runReaderT (runEitherT $ runOrchestrate a)
                         =<< ask

handler' :: Monad m
         => (Ex.SomeException -> OrchestrateT m a)
         -> Either Ex.SomeException a
         -> OrchestrateT m a
handler' _ (Right v) = return v
handler' f (Left e)  = f e

orchestrateEither :: Monad m
                  => Either Ex.SomeException a -> OrchestrateT m a
orchestrateEither = OrchestrateT . hoistEither

type Orchestrate   = OrchestrateT Identity
type OrchestrateIO = OrchestrateT IO

data Path = Path
          { _itemCollection :: !T.Text
          , _itemKey        :: !T.Text
          , _itemRef        :: !T.Text
          } deriving (Show)
$(makeLenses ''Path)

instance FromJSON Path where
    parseJSON (Object o) =   Path
                         <$> o .: "collection"
                         <*> o .: "key"
                         <*> o .: "ref"
    parseJSON _          =   mzero

data ResultList i = ResultList
                  { _resultCount :: !Int
                  , _resultList  :: ![i]
                  , _resultPrev  :: !(Maybe Location)
                  , _resultNext  :: !(Maybe Location)
                  } deriving (Show)
$(makeLenses ''ResultList)

instance FromJSON r => FromJSON (ResultList r) where
    parseJSON (Object o) =   ResultList
                         <$> o .:  "count"
                         <*> o .:  "results"
                         <*> o .:? "prev"
                         <*> o .:? "next"
    parseJSON _          =   mzero

data ResultItem p v = ResultItem
                    { _itemPath  :: !p
                    , _itemValue :: !v
                    } deriving (Show)
$(makeLenses ''ResultItem)

instance (FromJSON p, FromJSON v) => FromJSON (ResultItem p v) where
    parseJSON (Object o) =   ResultItem
                         <$> o .: "path"
                         <*> o .: "value"
    parseJSON _          =   mzero
