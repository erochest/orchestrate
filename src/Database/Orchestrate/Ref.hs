{-# LANGUAGE OverloadedStrings #-}


module Database.Orchestrate.Ref
    ( TombstoneItem(..)

    , getRef
    , listRef
    ) where


import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types           (Parser)
import qualified Data.HashMap.Strict        as M

import           Database.Orchestrate.Types


getRef :: (OrchestrateData v, FromJSON r) => v -> OrchestrateT m (Maybe r)
getRef = undefined

listRef :: Collection -> Key -> Maybe Int -> Maybe Int -> Bool
        -> OrchestrateT m [ResultList (TombstoneItem v)]
listRef = undefined

data TombstoneItem v = TombstoneItem { tombstonePath :: !Path
                                     , tombstoneTime :: !Timestamp
                                     }
                     | LiveItem      { livePath  :: !Path
                                     , liveValue :: !(Maybe v)
                                     , liveTime  :: !Timestamp
                                     }
                     deriving (Show)

emptyObject :: FromJSON v => Value -> Parser (Maybe v)
emptyObject o@(Object m) | M.null m  = return Nothing
                         | otherwise = parseJSON o
emptyObject _                        = return Nothing

instance FromJSON v => FromJSON (TombstoneItem v) where
    parseJSON (Object o) = do
        let path    = o .: "path"
            reftime = o .: "reftime"
        (Object p) <- o .: "path"
        tombstone  <- p .: "tombstone"
        case tombstone of
            Just (Bool True) -> TombstoneItem <$> path <*> reftime
            _                -> LiveItem <$> path <*> (emptyObject =<< o .: "value") <*> reftime
    parseJSON _          = mzero
