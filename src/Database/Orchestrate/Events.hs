{-# LANGUAGE OverloadedStrings #-}


module Database.Orchestrate.Events
    ( EventPath(..)
    , EventItem(..)
    , EventList

    , getEvent
    , createEvent
    , updateEvent
    , deleteEvent
    , listEvents
    ) where


import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.Text                  as T

import           Database.Orchestrate.Types


type EventType = T.Text

getEvent :: (OrchestrateData a, FromJSON b)
          => a -> EventType -> Timestamp -> Int
          -> OrchestrateT m (EventItem b a)
getEvent = undefined

createEvent :: (OrchestrateData a, ToJSON b)
            => a -> EventType -> b -> Maybe Timestamp
            -> OrchestrateT m Location
createEvent = undefined

updateEvent :: (OrchestrateData a, ToJSON b)
            => a -> b -> EventType -> Timestamp -> Int -> IfMatch
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

data EventPath = EventPath
               { eventPath     :: !Path
               , eventPathType :: !EventType
               , eventPathTime :: !Timestamp
               , eventPathOrd  :: !Int
               } deriving (Show)

instance FromJSON EventPath where
    parseJSON o'@(Object o) =   EventPath
                            <$> parseJSON o'
                            <*> o .: "type"
                            <*> o .: "timestamp"
                            <*> o .: "ordinal"
    parseJSON _             =   mzero

data EventItem a b = EventItem
                   { eventItem :: !(ResultItem EventPath a)
                   , eventTime :: !Timestamp
                   , eventOrd  :: !Int
                   } deriving (Show)

instance FromJSON a => FromJSON (EventItem a b) where
    parseJSON o'@(Object o) =   EventItem
                            <$> parseJSON o'
                            <*> o .: "timestamp"
                            <*> o .: "ordinal"
    parseJSON _             =   mzero

type EventList a b = ResultList (EventItem a b)
