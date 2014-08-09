{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wall #-}


module Database.Orchestrate.Utils
    ( ping
    , runO
    , runO'
    , baseUrl
    , withAuth'
    , withAuth
    , envSession
    , rot
    , ifMatch
    , ifMatch'
    , locationKey
    , locationRef
    , getLocation
    , rangeStart
    , rangeEnd
    , tshow
    ) where


import           Control.Applicative
import           Control.Error
import qualified Control.Exception            as Ex
import           Control.Lens
import           Control.Monad.Reader
import qualified Data.ByteString              as B
import           Data.Default
import           Data.Monoid
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as E
import           Network.HTTP.Types.Header
import           Network.Wreq
import           System.Environment

import           Database.Orchestrate.Network
import           Database.Orchestrate.Types


ping :: OrchestrateIO ()
ping = ask >>= restGet [] [] [] >>= checkResponse

runO :: Monad m => OrchestrateT m a -> RestSession -> m (Either Ex.SomeException a)
runO m s = runO' m $ over sessionOptions (withAuth $ s ^. sessionKey) s

runO' :: Monad m => OrchestrateT m a -> RestSession -> m (Either Ex.SomeException a)
runO' m = runReaderT (runEitherT $ runOrchestrate m)

baseUrl :: Monad m => OrchestrateT m T.Text
baseUrl = do
    RestSession{..} <- ask
    return $ mconcat [_sessionURL, "/v", tshow _sessionVersion]

withAuth' :: APIKey -> Options
withAuth' key = withAuth key defaults

withAuth :: APIKey -> Options -> Options
withAuth key o = o & auth .~ basicAuth (E.encodeUtf8 key) ""

envSession :: IO RestSession
envSession = do
    key <- T.pack <$> getEnv "ORCHESTRATE_API"
    return $ def & sessionKey .~ key
                 & over sessionOptions (withAuth key)

rot :: (a -> b -> c -> d) -> c -> a -> b -> d
rot f c a b = f a b c

ifMatch :: IfMatch -> [Header]
ifMatch = maybeToList . fmap ((,) "If-Match" . E.encodeUtf8)

ifMatch' :: IfMatch' -> [Header]
ifMatch' (IfMatch r)     = [("If-Match", E.encodeUtf8 r)]
ifMatch' (IfNoneMatch r) = [("If-None-Match", E.encodeUtf8 r)]
ifMatch' NoMatch         = []

-- locationKey :: (T.Text -> T.Text) -> T.Text -> T.Text -> T.Text
locationKey :: Prism' T.Text T.Text
locationKey = prism' (mappend "//") ((`atMay` 3) . T.split (=='/'))

locationRef :: Prism' T.Text T.Text
locationRef = prism' (mappend "////") ((`atMay` 5) . T.split (=='/'))

getLocation :: Response a -> T.Text
getLocation = E.decodeUtf8 . view (responseHeader "Location")

rangeStart :: FormValue a => B.ByteString -> RangeEnd a -> Maybe FormParam
rangeStart k (Inclusive r) = Just $ ("start" <> k) := r
rangeStart k (Exclusive r) = Just $ ("after" <> k) := r
rangeStart _ Open          = Nothing

rangeEnd :: FormValue a => B.ByteString -> RangeEnd a -> Maybe FormParam
rangeEnd k (Inclusive r) = Just $ ("end"    <> k) := r
rangeEnd k (Exclusive r) = Just $ ("before" <> k) := r
rangeEnd _ Open          = Nothing

tshow :: Show a => a -> T.Text
tshow = T.pack . show
