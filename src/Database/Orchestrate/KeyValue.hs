{-# LANGUAGE OverloadedStrings #-}


module Database.Orchestrate.KeyValue
    ( KVList

    , getKV
    , putKV
    , putV
    , postV
    , deleteKV
    , listKV
    ) where


import           Data.Aeson

import Database.Orchestrate.Types


getKV :: FromJSON v => Collection -> Key -> OrchestrateT m (Maybe v)
getKV = undefined

putKV :: OrchestrateData v => v -> IfMatch' -> OrchestrateT m Location
putKV = undefined

putV :: FromJSON v => Key -> v -> IfMatch' -> OrchestrateT m Location
putV = undefined

postV :: FromJSON v => v -> OrchestrateT m Location
postV = undefined

deleteKV :: OrchestrateData v => v -> IfMatch -> OrchestrateT m ()
deleteKV = undefined

listKV :: FromJSON v
       => Collection -> Maybe Int -> Range Key -> OrchestrateT m [KVList v]
listKV = undefined

type KVList v = ResultList (ResultItem Path v)
