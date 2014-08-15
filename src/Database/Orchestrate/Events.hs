{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Database.Orchestrate.Events
    ( EventPath(..)
    , eventPath
    , eventPathType
    , eventPathTime
    , eventPathOrd

    , EventItem(..)
    , eventItem
    , eventTime
    , eventOrd

    , EventList
    , EventType

    , getEvent
    , createEvent
    , updateEvent
    , deleteEvent
    , listEvents

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
import qualified Data.Text.Encoding         as E
import           Network.Wreq

import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils


type EventType = T.Text

data EventPath = EventPath
               { _eventPath     :: !Path
               , _eventPathType :: !EventType
               , _eventPathTime :: !Timestamp
               , _eventPathOrd  :: !Int
               } deriving (Show)
$(makeLenses ''EventPath)

instance FromJSON EventPath where
    parseJSON o'@(Object o) =   EventPath
                            <$> parseJSON o'
                            <*> o .: "type"
                            <*> o .: "timestamp"
                            <*> o .: "ordinal"
    parseJSON _             =   mzero


data EventItem a b = EventItem
                   { _eventItem :: !(ResultItem EventPath a)
                   , _eventTime :: !Timestamp
                   , _eventOrd  :: !Int
                   } deriving (Show)
$(makeLenses ''EventItem)

instance FromJSON a => FromJSON (EventItem a b) where
    parseJSON o'@(Object o) =   EventItem
                            <$> parseJSON o'
                            <*> o .: "timestamp"
                            <*> o .: "ordinal"
    parseJSON _             =   mzero

urlPath :: OrchestrateData a => a -> EventType -> Timestamp -> Int -> [T.Text]
urlPath a et ts o = [tableName a, dataKey a, "events", et, tshow ts, tshow o]

getEvent :: (OrchestrateData a, FromJSON b)
         => a -> EventType -> Timestamp -> Int
         -> OrchestrateIO (Maybe (EventItem b a))
getEvent a etype ts o =
    join . fmap (decode . (^.responseBody)) <$> api404 [] up [] getWith
    where up = urlPath a etype ts o

createEvent :: (OrchestrateData a, ToJSON b)
            => a -> EventType -> b -> Maybe Timestamp
            -> OrchestrateIO Location
createEvent a eType e mts =
    getLocation <$> api [] urlPath [] (rot postWith (toJSON e))
    where urlPath =  [tableName a, dataKey a, "events", eType]
                  ++ maybeToList (fmap tshow mts)

updateEvent :: (OrchestrateData a, ToJSON b)
            => a -> EventType -> b -> Timestamp -> Int -> IfMatch
            -> OrchestrateIO Location
updateEvent a eType e ts o m =
    getLocation <$> api (ifMatch m) up [] (rot putWith (toJSON e))
    where up = urlPath a eType ts o

deleteEvent :: OrchestrateData a
            => a -> EventType -> Timestamp -> Int -> IfMatch
            -> OrchestrateIO ()
deleteEvent a eType ts o m =
    void $ apiCheck (ifMatch m) up ["purge" := ("true" :: T.Text)] deleteWith
    where up = urlPath a eType ts o

listEvents :: (OrchestrateData a, FromJSON b)
           => a -> EventType -> Maybe Int -> Range (Timestamp, Maybe Int)
           -> OrchestrateIO (EventList b a)
listEvents a eType limit (start, end) = apiCheckDecode [] up ps getWith
    where up = [tableName a, dataKey a, "events", eType]
          ps = catMaybes [ ("limit" :=) <$> limit
                         , rangeStart "Event" $ fmap eventRange start
                         , rangeEnd   "Event" $ fmap eventRange end
                         ]
          eventRange (ts, mint) = tshow ts <> maybe mempty (("/" <>) . tshow) mint

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

-- /v0/collection/key/events/type/1398286518286/6
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

type EventList a b = ResultList (EventItem a b)
