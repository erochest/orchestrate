{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{- |
This module implements the <http://orchestrate.io/api/events API calls> for
working with Orchestrate events.
-}


module Database.Orchestrate.Events
    (
    -- * API Functions
      getEvent
    , createEvent
    , updateEvent
    , deleteEvent
    , listEvents

    -- * Types

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

    -- * Location Conversion Functions
    , locationEventItem
    , eventItemLocation
    ) where


import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                  as T
import           Network.Wreq

import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils


-- | The kind of event represented.
type EventType = T.Text

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

instance FromJSON a => FromJSON (EventItem a b) where
    parseJSON o'@(Object o) =   EventItem
                            <$> parseJSON o'
                            <*> o .: "timestamp"
                            <*> o .: "ordinal"
    parseJSON _             =   mzero

urlPath :: OrchestrateData a => a -> EventType -> Timestamp -> Int -> [T.Text]
urlPath a et ts o = [tableName a, dataKey a, "events", et, tshow ts, tshow o]

-- | This retrieves a single event. See <http://orchestrate.io/api/events the API documentation>
-- for more information.
--
-- For example, this retrieves a transaction event.
--
-- > getEvent data "transaction" 784111777000 79
getEvent :: (OrchestrateData a, FromJSON b)
         => a               -- ^ The data that the event is associated with.
         -> EventType       -- ^ The kind of event.
         -> Timestamp       -- ^ The event's timestamp.
         -> Int             -- ^ The event's ordinal number.
         -> OrchestrateIO (Maybe (EventItem b a))   -- ^ 'Just' the event or 'Nothing'.
getEvent a etype ts o =
    join . fmap (decode . (^.responseBody)) <$> api404 [] up [] getWith
    where up = urlPath a etype ts o

-- | This creates an event and returns its 'Location'. See
-- <http://orchestrate.io/api/events API document> for more information.
--
-- For example, this creates a transaction, using the current time for the
-- timestamp.
--
-- > createEvent data "transaction" transactionData Nothing
createEvent :: (OrchestrateData a, ToJSON b)
            => a                        -- ^ The data that the event is associated with.
            -> EventType                -- ^ The kind of event.
            -> b                        -- ^ The event data.
            -> Maybe Timestamp          -- ^ The event's timestamp. If not given, the current time is used.
            -> OrchestrateIO Location   -- ^ Returns the event's location.
createEvent a eType e mts =
    getLocation <$> api [] url [] (rot postWith (toJSON e))
    where url =  [tableName a, dataKey a, "events", eType]
              ++ maybeToList (fmap tshow mts)

-- | This updates the data for an event. The storage is keyed by type,
-- timestamp, and ordinal, so the event data can change. See the
-- <http://orchestrate.io/api/events API documentation> for more
-- information.
--
-- For example:
--
-- > updateEvent data "transaction" updatedTransactionData
-- >     <$> loc ^? locationTimestamp
-- >     <*> loc ^? locationOrdinal
updateEvent :: (OrchestrateData a, ToJSON b)
            => a                -- ^ The data that the event is associated with.
            -> EventType        -- ^ The kind of event.
            -> b                -- ^ The updated event data.
            -> Timestamp        -- ^ The timestamp for the event.
            -> Int              -- ^ The ordinal for the event.
            -> IfMatch'         -- ^ If given, will only succeed if the ref matches
                                -- the event's currently stored ref.
            -> OrchestrateIO Location   -- ^ Returns the 'Location' for the new event data.
updateEvent a eType e ts o m =
    getLocation <$> api (ifMatch' m) up [] (rot putWith (toJSON e))
    where up = urlPath a eType ts o

-- | This deletes an event. See the <http://orchestrate.io/api/events API documentation>
-- for more information.
--
-- For example:
--
-- > deleteEvent data "transaction"
-- >     <$> loc ^? locationTimestamp
-- >     <*> loc ^? locationOrdinal
deleteEvent :: OrchestrateData a
            => a                    -- ^ The data that the event is associated with.
            -> EventType            -- ^ The kind of event.
            -> Timestamp            -- ^ The timestamp for the event.
            -> Int                  -- ^ The ordinal for the event.
            -> IfMatch'             -- ^ If given, will only succeed if the ref matches
                                    -- the event's currently stored ref.
            -> OrchestrateIO ()
deleteEvent a eType ts o m =
    void $ apiCheck (ifMatch' m) url ["purge" := ("true" :: T.Text)] deleteWith
    where url = urlPath a eType ts o

-- | This lists all the events within a given range for a data. See the
-- <http://orchestrate.io/api/events API documentation> for more
-- information.
--
-- For example:
--
-- > listEvents data "transaction" (Just 25) (Open, Open)
listEvents :: (OrchestrateData a, FromJSON b)
           => a                                 -- ^ The data the events are to associated with.
           -> EventType                         -- ^ The type of events to retrieve.
           -> Maybe Int                         -- ^ The maximum number of event data to return.
                                                -- The default is 10, and the maximum is 100.
           -> Range (Timestamp, Maybe Int)      -- ^ The range of events to retrieve. Each range
                                                -- is the timestamp and maybe the ordinal.
           -> OrchestrateIO (EventList b a)
listEvents a eType limit (start, end) = apiCheckDecode [] up ps getWith
    where up = [tableName a, dataKey a, "events", eType]
          ps = catMaybes [ ("limit" :=) <$> limit
                         , rangeStart "Event" $ fmap eventRange start
                         , rangeEnd   "Event" $ fmap eventRange end
                         ]
          eventRange (ts, mint) = tshow ts <> maybe mempty (("/" <>) . tshow) mint

-- | This takes a 'Location' and an event datum and returns the 'EventItem'
-- representing it.
locationEventItem :: Location -> a -> Maybe (EventItem a b)
locationEventItem loc a = EventItem <$> resultItem a <*> timestamp <*> ordinal
    where timestamp    = loc ^? locationTimestamp
          ordinal      = loc ^? locationOrdinal
          resultItem x = (`ResultItem` x) <$> ePath
          ePath        =   EventPath
                       <$> path
                       <*> loc ^? locationType
                       <*> timestamp
                       <*> ordinal
          path         =   Path
                       <$> loc ^? locationCollection
                       <*> loc ^? locationKey
                       <*> loc ^? locationRef

-- | This takes an 'EventItem' and returns the datum and 'Location'
-- associated with that item.
eventItemLocation :: Monad m => EventItem a b -> OrchestrateT m (a, Location)
eventItemLocation ei = do
    v <- view sessionVersion
    let epath = ei ^. eventItem . itemPath
        path  = epath ^. eventPath
        loc   = T.intercalate "/" [ ""
                                  , "v" <> tshow v
                                  , path ^. itemCollection
                                  , path ^. itemKey
                                  , "events"
                                  , epath ^. eventPathType
                                  , tshow $ epath ^. eventPathTime
                                  , tshow $ epath ^. eventPathOrd
                                  ]
    return (ei ^. eventItem . itemValue, loc)
