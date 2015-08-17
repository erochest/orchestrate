{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -Wall #-}


-- TODO: Clean this up some. Eeek.

module Database.Orchestrate.Types
    (
    -- * General Types
    -- ** Aliases
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
    , IfMatch'
    , IfMatch(..)
    -- ** Ranges
    , Range
    , RangeEnd(..)

    -- * Key/Value Types
    , KVList

    -- * Ref Types
    , TombstoneItem(..)

    -- ** Type Lenses and Prisms
    , _TombstoneItem
    , _LiveItem
    , livePath
    , liveTime
    , liveValue
    , tombstonePath
    , tombstoneTime

    -- * Event Types
    -- ** Type Aliases
    , EventList
    , EventType

    -- ** Event Location
    , EventPath(..)
    , eventPath
    , eventPathType
    , eventPathTime
    , eventPathOrd

    -- ** Event Data
    , EventItem(..)
    , eventItem
    , eventTime
    , eventOrd

    -- * Graph Types
    , RelKind
    , RelList

    -- * Search Types
    , QueryText

    -- ** List of Search Results
    , SearchList(..)
    , searchResults
    , searchTotal

    -- ** Single Search Result
    , SearchItem(..)
    , searchItem
    , searchScore

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

    -- * Re-exports
    -- ** Accessing Data
    , ask
    , asks
    -- ** Throwing and Handling Errors
    , throwError
    , catchError
    ) where


import           Control.Error
import qualified Control.Exception         as Ex
import           Control.Lens              hiding ((.=))
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Types           (Parser)
import           Data.Default
import qualified Data.HashMap.Strict        as M
import qualified Data.Text                 as T
import           Network.Wreq


type APIKey     = T.Text
type Collection = T.Text
type Key        = T.Text
type Ref        = T.Text
type Timestamp  = Integer
type Location   = T.Text
type IfMatch'   = Maybe Ref
type Limit      = Int
type Offset     = Int
type EventType  = T.Text
type RelKind    = T.Text
type QueryText  = T.Text

-- | This represents a function that makes a call to the Orchestrate API
-- server. It takes 'Options', a URL 'String', and returns a 'Response'.
type RestCall a = Options -> String -> IO (Response a)

-- | A richer type than 'IfMatch' for specifying conditional calls.
data IfMatch = IfMatch Ref       -- ^ Only perform the action if the ref does exist.
             | IfNoneMatch Ref   -- ^ Only perform the action if the ref does not exist.
             | NoMatch           -- ^ Always perform the action.
             deriving (Show)

-- | This is a range tuple. Each end can be specified separately.
type Range a = (RangeEnd a, RangeEnd a)

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
    { runOrchestrate :: ExceptT Ex.SomeException (ReaderT Session m) a }
    deriving (Functor, Applicative, Monad)

instance MonadTrans OrchestrateT where
    lift = OrchestrateT . lift . lift

instance MonadIO m => MonadIO (OrchestrateT m) where
    liftIO = OrchestrateT . liftIO . liftIO

instance Monad m => MonadReader Session (OrchestrateT m) where
    ask     = OrchestrateT . lift $ ask
    local f = OrchestrateT . local f . runOrchestrate

-- TODO: Need to define this for other monad classes.

instance Monad m => MonadError Ex.SomeException (OrchestrateT m) where
    throwError = OrchestrateT . ExceptT . return . Left
    catchError a handler =   join
                         .   fmap (handler' handler)
                         .   lift
                         .   runReaderT (runExceptT $ runOrchestrate a)
                         =<< ask

handler' :: Monad m
         => (Ex.SomeException -> OrchestrateT m a)
         -> Either Ex.SomeException a
         -> OrchestrateT m a
handler' _ (Right v) = return v
handler' f (Left e)  = f e

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

instance ToJSON Path where
    toJSON (Path c k r) = object [ "collection" .= c
                                 , "key"        .= k
                                 , "ref"        .= r
                                 ]

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

instance (ToJSON p, ToJSON v) => ToJSON (ResultItem p v) where
    toJSON (ResultItem p v) = object ["path" .= p, "value" .= v]

-- | The data necessary to access an event.
data EventPath = EventPath
               { _eventPath     :: !Path            -- ^ The base 'Path' to this data.
               , _eventPathType :: !EventType       -- ^ The kind of event.
               , _eventPathTime :: !Timestamp       -- ^ The event's timestamp.
               , _eventPathOrd  :: !Int             -- ^ The event's ordinal number.
               } deriving (Show)
$(makeLenses ''EventPath)

instance FromJSON EventPath where
    parseJSON o'@(Object o) =   EventPath
                            <$> parseJSON o'
                            <*> o .: "type"
                            <*> o .: "timestamp"
                            <*> o .: "ordinal"
    parseJSON _             =   mzero

instance ToJSON EventPath where
    toJSON (EventPath p et ts o) =
        Object $ case toJSON p of
            Object m -> m `M.union` epm
            _        -> epm
        where epm = M.fromList [ ("type",      toJSON et)
                               , ("timestamp", toJSON ts)
                               , ("ordinal",   toJSON o)
                               ]

-- | One item in an 'EventList'.
--
-- This data type uses two parameters:
--
-- [@a@] The type of data being stored for the event.
--
-- [@b@] A phantom type for the type of data associated with the event.
-- This data must also be stored in Orchestrate using the
-- "Database.Orchestrate.KeyValue" API.
data EventItem a b = EventItem
                   { _eventItem :: !(ResultItem EventPath a)    -- ^ The data itself and the path to it.
                   , _eventTime :: !Timestamp                   -- ^ The event's timestamp.
                   , _eventOrd  :: !Int                         -- ^ The event's ordinal number.
                   } deriving (Show)
$(makeLenses ''EventItem)

instance FromJSON a => FromJSON (EventItem a b) where
    parseJSON o'@(Object o) =   EventItem
                            <$> parseJSON o'
                            <*> o .: "timestamp"
                            <*> o .: "ordinal"
    parseJSON _             =   mzero

-- | 'TombstoneItem' data represents the data values in the database.
data TombstoneItem v =
    -- | 'TombstoneItem' data are no longer alive. They are simply markers
    -- for deleted data.
      TombstoneItem { _tombstonePath :: !Path       -- ^ The path to the deleted data.
                    , _tombstoneTime :: !Timestamp  -- ^ The timestamp of this data.
                    }
    -- | 'LiveItem' data are still in the database.
    | LiveItem      { _livePath  :: !Path           -- ^ The path to the data.
                    , _liveValue :: !(Maybe v)      -- ^ If values are requested,
                                                    -- this will contain the data.
                    , _liveTime  :: !Timestamp      -- ^ The timestamp for the data.
                    }
                    deriving (Show)
$(makeLenses ''TombstoneItem)

-- | A 'Prism'' into data created with the 'TombstoneItem' constructor.
_TombstoneItem :: Prism' (TombstoneItem v) (TombstoneItem v)
_TombstoneItem = prism' id $ \i ->
    case i of
        TombstoneItem{} -> Just i
        LiveItem{}      -> Nothing

-- | A 'Prism'' into data created with the 'LiveItem' constructor.
_LiveItem :: Prism' (TombstoneItem v) (TombstoneItem v)
_LiveItem = prism' id $ \i ->
    case i of
        TombstoneItem{} -> Nothing
        LiveItem{}      -> Just i

emptyObject :: FromJSON v => Maybe Value -> Parser (Maybe v)
emptyObject (Just o@(Object m)) | M.null m  = return Nothing
                                | otherwise = parseJSON o
emptyObject _                               = return Nothing

instance FromJSON v => FromJSON (TombstoneItem v) where
    parseJSON (Object o) = do
        let path    = o .:  "path"
            reftime = o .:  "reftime"
        (Object p) <- o .:  "path"
        tombstone  <- p .:? "tombstone"
        case tombstone of
            Just (Bool True) -> TombstoneItem <$> path <*> reftime
            _                -> LiveItem <$> path <*> (emptyObject =<< o .:? "value") <*> reftime
    parseJSON _          = mzero

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

-- | A list of events returned by 'listEvents'.
--
-- This data type uses two parameters:
--
-- [@a@] The type of data being stored for the event.
--
-- [@b@] A phantom type for the type of data associated with the event.
-- This data must also be stored in Orchestrate using the
-- "Database.Orchestrate.KeyValue" API.
type EventList a b = ResultList (EventItem a b)

-- | A list of edges returned by 'getRel'.
--
-- This datatype uses two parameters:
--
-- [@a@] The data type for the edge's origin node.
-- [@b@] The data type for the edge's target node.
type RelList a b = ResultList (ResultItem Path b)

-- | A list of data returned by 'listV'.
--
-- [@v@] The type of the data contained in the list.
type KVList v = ResultList (ResultItem Path v)
