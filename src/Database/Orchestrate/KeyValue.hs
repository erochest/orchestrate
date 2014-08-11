{-# LANGUAGE FlexibleContexts      #-}
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
import qualified Control.Exception            as Ex
import           Control.Lens
import           Control.Monad                (join)
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class       (liftIO)
import           Data.Aeson
import           Data.Monoid
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as E
import           Network.HTTP.Client          hiding (responseBody)
import           Network.HTTP.Types.Status    hiding (statusCode)
import           Network.Wreq

import           Database.Orchestrate.Network
import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils


getKV :: FromJSON v => Collection -> Key -> OrchestrateIO (Maybe v)
getKV c k = do
    s <- ask
    -- I'm not convinced this does what I think it does.
    er <- liftIO $ Ex.catchJust
        Ex.fromException
        (fmapL filterStatusCode <$> runO' (api [c, k] [] Nothing getWith) s)
        handler
    case er of
        Right r -> checkResponse r >> return (decode $ r ^. responseBody)
        Left (Just (StatusCodeException (Status 404 _) _ _)) -> return Nothing
        Left (Just e) -> throwSome e
        Left Nothing  -> throwSome $ StatusCodeException status418 [] mempty

    where
        handler :: HttpException -> IO (Either (Maybe HttpException) a)
        handler e@(StatusCodeException{}) = return . Left $ Just e
        handler e = Ex.throw e

        onlyStatusCode :: HttpException -> Maybe HttpException
        onlyStatusCode e@(StatusCodeException{}) = Just e
        onlyStatusCode _ = Nothing

        filterStatusCode :: Ex.SomeException -> Maybe HttpException
        filterStatusCode = join . fmap onlyStatusCode . Ex.fromException

        throwSome :: (Ex.Exception e, Monad m, MonadError Ex.SomeException m)
                  => e -> m a
        throwSome = throwError . Ex.SomeException

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
purgeV k v m =
        api [tableName v, k] ["purge" := ("true" :: T.Text)]
            (Just $ ifMatch m) deleteWith
    >>= checkResponse

listKV :: FromJSON v
       => Collection -> Maybe Int -> Range Key -> OrchestrateIO (KVList v)
listKV c limit (start, end) = do
    r <- api [c] ps Nothing getWith
    checkResponse r
    orchestrateEither . note err . decode $ r ^. responseBody
    where ps = catMaybes [ Just $ "limit" := limit
                         , rangeStart "Key" start
                         , rangeEnd   "Key" end
                         ]
          err = Ex.SomeException $ Ex.ErrorCall "Invalid JSON returned."

type KVList v = ResultList (ResultItem Path v)
