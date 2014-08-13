{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}


module Database.Orchestrate.KeyValue
    ( KVList

    , getKV
    , putKV
    , putV
    , postV
    , deleteKV
    , deleteV
    , purgeKV
    , purgeV
    , listKV
    ) where


import           Control.Applicative
import           Control.Arrow
import           Control.Error
import           Control.Lens
import           Control.Monad                (join, void)
import           Data.Aeson
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as E
import           Network.Wreq

import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils


-- TODO: Use more-standard names.

getKV :: FromJSON v => Collection -> Key -> OrchestrateIO (Maybe v)
getKV c k =
    join . fmap (decode . (^.responseBody)) <$> api404 [] [c, k] [] getWith

putKV :: OrchestrateData v => v -> IfMatch' -> OrchestrateIO Location
putKV v = putV (dataKey v) v

putV :: OrchestrateData v => Key -> v -> IfMatch' -> OrchestrateIO Location
putV k v m =   E.decodeUtf8 . view (responseHeader "Location")
           <$> api (ifMatch' m) [tableName v, k] [] (rot putWith v')
    where v' = toJSON v

postV :: OrchestrateData v => v -> OrchestrateIO (Location, Maybe Key)
postV v =   (id &&& firstOf locationKey) . E.decodeUtf8 . view (responseHeader "Location")
        <$> api [] [tableName v] [] (rot postWith (toJSON v))

deleteKV :: OrchestrateData v => v -> IfMatch -> OrchestrateIO ()
deleteKV v = deleteV (dataKey v) v

deleteV :: OrchestrateData v => Key -> v -> IfMatch -> OrchestrateIO ()
deleteV k v m = void $ apiCheck (ifMatch m) [tableName v, k] [] deleteWith

purgeKV :: OrchestrateData v => v -> IfMatch -> OrchestrateIO ()
purgeKV v = purgeV (dataKey v) v

purgeV :: OrchestrateData v => Key -> v -> IfMatch -> OrchestrateIO ()
purgeV k v m =
    void $ apiCheck (ifMatch m) [tableName v, k]
                    ["purge" := ("true" :: T.Text)] deleteWith

listKV :: FromJSON v
       => Collection -> Maybe Int -> Range Key -> OrchestrateIO (KVList v)
listKV c limit (start, end) = apiCheckDecode [] [c] ps getWith
    where ps = catMaybes [ Just $ "limit" := limit
                         , rangeStart "Key" start
                         , rangeEnd   "Key" end
                         ]

type KVList v = ResultList (ResultItem Path v)
