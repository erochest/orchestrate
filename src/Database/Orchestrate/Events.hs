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
import qualified Data.Text                  as T

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


getEvent :: (OrchestrateData a, FromJSON b)
         => a -> EventType -> Timestamp -> Int
         -> OrchestrateT m (Maybe (EventItem b a))
getEvent = undefined

createEvent :: (OrchestrateData a, ToJSON b)
            => a -> EventType -> b -> Maybe Timestamp
            -> OrchestrateT m Location
createEvent = undefined

updateEvent :: (OrchestrateData a, ToJSON b)
            => a -> EventType -> b -> Timestamp -> Int -> IfMatch
            -> OrchestrateT m Location
updateEvent = undefined

deleteEvent :: OrchestrateData a
            => a -> EventType -> Timestamp -> Int -> IfMatch
            -> OrchestrateT m ()
deleteEvent = undefined

listEvents :: (OrchestrateData a, ToJSON b)
           => a -> EventType -> Maybe Int -> Range (Timestamp, Maybe Int)
           -> OrchestrateT m (EventList b a)
listEvents = undefined

locationEventItem :: Location -> a -> Maybe (EventItem a b)
locationEventItem loc a = EventItem <$> resultItem a <*> timestamp <*> ordinal
    where timestamp    = loc ^? locationTimestamp
          ordinal      = loc ^? locationOrdinal
          resultItem x = (`ResultItem` x) <$> ePath
          ePath        = EventPath
                       <$> path
                       <*> loc ^? locationType
                       <*> timestamp
                       <*> ordinal
          path         =  Path
                       <$> loc ^? locationCollection
                       <*> loc ^? locationKey
                       <*> loc ^? locationRef

eventItemLocation :: Monad m => EventItem a b -> OrchestrateT m (a, Location)
eventItemLocation ei = undefined

type EventList a b = ResultList (EventItem a b)
