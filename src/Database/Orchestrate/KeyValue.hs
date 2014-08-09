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
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Monoid
import qualified Data.Text                    as T
import           Network.HTTP.Client          hiding (responseBody)
import           Network.HTTP.Types.Status    hiding (statusCode)
import           Network.Wreq

import           Database.Orchestrate.Network
import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils


getKV :: FromJSON v => Collection -> Key -> OrchestrateIO (Maybe v)
getKV c k = do
    s  <- ask
    -- I don't think this is actually doing what I'm thinking it's doing.
    er <- liftIO $ Ex.catchJust
        Ex.fromException
        (fmapL filterStatusCode <$> runO' (restGet [] [c, k] [] s) s)
        handler
    case er of
        Right r -> checkResponse r >> return (decode $ r ^. responseBody)
        Left (Just (StatusCodeException (Status 404 _) _ _)) -> return Nothing
        Left (Just e) -> throwSome e
        Left Nothing  -> throwSome $ StatusCodeException status418 [] mempty
    where handler :: HttpException -> IO (Either (Maybe HttpException) a)
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
putV k v m =   fmap getLocation
           .   restPut (ifMatch' m) [tableName v, k] (toJSON v)
           =<< ask

postV :: OrchestrateData v => v -> OrchestrateIO (Location, Maybe Key)
postV v =   fmap ((id &&& firstOf locationKey) . getLocation)
        .  restPost [] [tableName v] (toJSON v)
        =<< ask

deleteKV :: OrchestrateData v => v -> IfMatch -> OrchestrateIO ()
deleteKV v = deleteV (dataKey v) v

deleteV :: OrchestrateData v => Key -> v -> IfMatch -> OrchestrateIO ()
deleteV k v m =   ask
              >>= restDelete (ifMatch m) [tableName v, k] []
              >>= checkResponse

purgeKV :: OrchestrateData v => v -> IfMatch -> OrchestrateIO ()
purgeKV v = purgeV (dataKey v) v

purgeV :: OrchestrateData v => Key -> v -> IfMatch -> OrchestrateIO ()
purgeV k v m =   ask
             >>= restDelete (ifMatch m) [tableName v, k] ["purge" := ("true" :: T.Text)]
             >>= checkResponse

listKV :: FromJSON v
       => Collection -> Maybe Int -> Range Key -> OrchestrateIO (KVList v)
listKV c limit (start, end) = do
    r <- restGet [] [c] ps =<< ask
    checkResponse r
    orchestrateEither . note err . decode $ r ^. responseBody
    where ps = catMaybes [ Just $ "limit" := limit
                         , rangeStart "Key" start
                         , rangeEnd   "Key" end
                         ]
          err = Ex.SomeException $ Ex.ErrorCall "Invalid JSON returned."

type KVList v = ResultList (ResultItem Path v)
