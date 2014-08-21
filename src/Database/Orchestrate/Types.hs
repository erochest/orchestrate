{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -Wall #-}


-- TODO: Use types from wreq for params, headers, etc.
-- TODO: Clean this up some. Eeek.

module Database.Orchestrate.Types
    (
    -- * Types
    -- ** General Aliases
      APIKey
    , Collection
    , Key
    , Ref
    , Timestamp
    , Location
    , RestCall
    -- ** Result Limits
    , Limit
    , Offset
    -- ** Conditional API Calls
    , IfMatch
    , IfMatch'(..)
    -- ** Ranges
    , Range
    , RangeEnd(..)

    -- * Session
    , Session(..)
    , sessionURL
    , sessionKey
    , sessionVersion
    , sessionOptions

    -- * Orchestrate Types
    -- ** Data
    , OrchestrateData(..)
    -- ** Monad Types
    , OrchestrateT(..)
    , OrchestrateIO
    , Orchestrate

    -- * Type Utilities
    -- ** Lifting
    , orchestrateEither
    , io
    -- ** Accessing Data
    , ask
    , asks
    -- ** Throwing and Handling Errors
    , throwError
    , catchError

    -- * Result Types
    -- ** Lists of Results
    , ResultList(..)
    , resultCount
    , resultList
    , resultPrev
    , resultNext
    -- ** Individual Results
    , ResultItem(..)
    , itemPath
    , itemValue
    -- ** Rich Item Locations (Paths)
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

-- | This represents a function that makes a call to the Orchestrate API
-- server. It takes 'Options', a URL 'String', and returns a 'Response'.
type RestCall a = Options -> String -> IO (Response a)

-- TODO: flip which one has the prime mark.

-- | A richer type than 'IfMatch' for specifying conditional calls.
data IfMatch'   = IfMatch Ref       -- ^ Only perform the action if the ref does exist.
                | IfNoneMatch Ref   -- ^ Only perform the action if the ref does not exist.
                | NoMatch           -- ^ Always perform the action.
                deriving (Show)

-- | This is a range tuple. Each end can be specified separately.
type Range a    = (RangeEnd a, RangeEnd a)

-- | This represents the end of a range.
data RangeEnd a = Inclusive a   -- ^ The end should be inclusive. I.e., it should include @a@.
                | Exclusive a   -- ^ The end should be exclusive. I.e., it should not include @a@.
                | Open          -- ^ There is no bound on this end.
                deriving (Show, Functor)

-- | The data for a session with the Orchestrate database.
data Session = Session
             { _sessionURL     :: !T.Text       -- ^ The base URL for the Orchestrate API.
             , _sessionKey     :: !APIKey       -- ^ The API key for accessing the API.
             , _sessionVersion :: !Int          -- ^ The version of the API.
             , _sessionOptions :: !Options      -- ^ The baseline set of 'Options' for making
                                                -- wreq calls. This includes the API key.
             } deriving (Show)
$(makeLenses ''Session)

instance Default Session where
    def = Session "https://api.orchestrate.io" "" 0
        $ defaults & header "Content-Type" .~ ["application/json"]
                   & header "Accept"       .~ ["application/json"]

-- | This is a class for data that can be stored in an Orchestrate
-- <http://orchestrate.io/api/keyvalue Key/Value> store. See
-- "Database.Orchestrate.KeyValue" for where this is used.
class (ToJSON a, FromJSON a) => OrchestrateData a where
    -- | This is the name of the collection to store this data type in.
    tableName :: a -> Collection
    -- | This is the key to store the value in.
    dataKey   :: a -> Key

-- | The type for the Orchestrate monad. All interactions with the
-- Orchestrate API are run in this monad. It combines a reader over
-- 'Session' data with error handling using 'EitherT' 'Ex.SomeException'.
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

-- | Lifts an IO action into the 'OrchestrateT' monad.
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

-- | Lifts an 'Either' value into the 'OrchestrateT' monad.
orchestrateEither :: Monad m
                  => Either Ex.SomeException a -> OrchestrateT m a
orchestrateEither = OrchestrateT . hoistEither

-- | 'OrchestrateT' over 'Identity'. Only the most useless monad ever.
type Orchestrate   = OrchestrateT Identity

-- | 'OrchestrateT' over 'IO', the way God intended.
type OrchestrateIO = OrchestrateT IO

-- | This represents the unique access information for a value in the
-- store.
data Path = Path
          { _itemCollection :: !Collection  -- ^ The collection containing the data.
          , _itemKey        :: !Key         -- ^ The data's key in the collection.
          , _itemRef        :: !Ref         -- ^ The reference to the current version
                                            -- of the value.
          } deriving (Show)
$(makeLenses ''Path)

instance FromJSON Path where
    parseJSON (Object o) =   Path
                         <$> o .: "collection"
                         <*> o .: "key"
                         <*> o .: "ref"
    parseJSON _          =   mzero

-- | A parameterized list of results returned by an API call.
--
-- [@i@] the type of the data contained.
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

-- | A parameterized single item returned in a collection by an API call.
--
-- [@p@] the type of the path data to this data.
-- [@v@] the type of the value for this data.
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
