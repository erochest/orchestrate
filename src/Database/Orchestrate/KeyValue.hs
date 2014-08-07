{-# LANGUAGE OverloadedStrings #-}


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
import           Data.Aeson
import           Data.Monoid
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as E
import           Network.Wreq

import           Database.Orchestrate.Network
import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils


getKV :: FromJSON v => Collection -> Key -> OrchestrateIO (Maybe v)
getKV c k = do
    -- Le sigh. Evidently wreq throws an error on 404 below.
    er <- api' [c, k] [] Nothing getWith
    case er of
        Right r -> checkResponse r >> return (decode $ r ^. responseBody)
        Left  s -> case s ^. statusCode of
                       404 -> return Nothing
                       _   -> throwError . T.pack $ show s

putKV :: OrchestrateData v => v -> IfMatch' -> OrchestrateIO Location
putKV v = putV (dataKey v) v

putV :: OrchestrateData v => Key -> v -> IfMatch' -> OrchestrateIO Location
putV k v m =   E.decodeUtf8 . view (responseHeader "Location")
           <$> api [tableName v, k] [] m' (rot putWith v')
    where m' = Just $ ifMatch' m
          v' = toJSON v

postV :: OrchestrateData v => v -> OrchestrateIO (Location, Maybe Key)
postV v =   (id &&& firstOf locationKey) . E.decodeUtf8 . view (responseHeader "Location")
        <$> api [tableName v] [] Nothing (rot postWith (toJSON v))

deleteKV :: OrchestrateData v => v -> IfMatch -> OrchestrateIO ()
deleteKV v = deleteV (dataKey v) v

deleteV :: OrchestrateData v => Key -> v -> IfMatch -> OrchestrateIO ()
deleteV k v m =   api [tableName v, k] [] (Just $ ifMatch m) deleteWith
              >>= checkResponse

purgeKV :: OrchestrateData v => v -> IfMatch -> OrchestrateIO ()
purgeKV v = purgeV (dataKey v) v

purgeV :: OrchestrateData v => Key -> v -> IfMatch -> OrchestrateIO ()
purgeV k v m =   api [tableName v, k] ["purge=true"] (Just $ ifMatch m) deleteWith
             >>= checkResponse

listKV :: FromJSON v
       => Collection -> Maybe Int -> Range Key -> OrchestrateIO (KVList v)
listKV c limit (start, end) = do
    r <- api [c] ps Nothing getWith
    checkResponse r
    orchestrateEither . note "Invalid JSON returned." . decode $ r ^. responseBody
    where ps = catMaybes [ ("limit=" <>) . T.pack . show <$> limit
                         , rangeStart "Key" start
                         , rangeEnd   "Key" end
                         ]

type KVList v = ResultList (ResultItem Path v)
