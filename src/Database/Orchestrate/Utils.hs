{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wall #-}


module Database.Orchestrate.Utils
    ( ping
    , runO
    , runO'
    , baseUrl
    , buildUrl
    , withAuth'
    , withAuth
    , api
    , api'
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
import           Control.Exception            as Ex
import           Control.Lens
import           Control.Monad.Reader
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Builder      as B
import qualified Data.ByteString.Lazy         as BSL
import           Data.CaseInsensitive         hiding (map)
import           Data.Default
import qualified Data.List                    as L
import           Data.Monoid
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as E
import           Network.HTTP.Client
import           Network.Wreq
import           Network.Wreq.Types           hiding (auth)
import           System.Environment

import           Database.Orchestrate.Network
import           Database.Orchestrate.Types


ping :: OrchestrateIO ()
ping = checkResponse =<< api [] [] Nothing headWith

runO :: Monad m => OrchestrateT m a -> Session -> m (Either Ex.SomeException a)
runO m s = runO' m $ over sessionOptions (withAuth $ s ^. sessionKey) s

runO' :: Monad m => OrchestrateT m a -> Session -> m (Either Ex.SomeException a)
runO' m = runReaderT (runEitherT $ runOrchestrate m)

baseUrl :: Monad m => OrchestrateT m T.Text
baseUrl = do
    Session{..} <- ask
    return $ mconcat [_sessionURL, "/v", tshow _sessionVersion]

withAuth' :: APIKey -> Options
withAuth' key = withAuth key defaults

withAuth :: APIKey -> Options -> Options
withAuth key o = o & auth .~ basicAuth (E.encodeUtf8 key) ""

buildUrl :: Monad m => [T.Text] -> [FormParam] -> OrchestrateT m String
buildUrl paths parms = do
    url <- view sessionURL
    v   <- view sessionVersion
    let parms' = if L.null parms
                     then mempty
                     else mappend "?"
                            . mconcat
                            . L.intersperse "&"
                            . map (uncurry $ jn "=")
                            . over (traverse . both) B.byteString
                            $ map toPair parms
        paths' = foldr (jn "/" . B.byteString . E.encodeUtf8) parms' paths
    return . T.unpack
           . E.decodeUtf8
           . BSL.toStrict
           . B.toLazyByteString
           $ mconcat [ B.byteString $ E.encodeUtf8 url
                     , "/v", B.intDec v
                     , "/", paths'
                     ]
    where jn j x y = x <> j <> y
          toPair (k := v) = (k, renderFormValue v)

api :: [T.Text] -> [FormParam] -> Maybe (Options -> Options)
    -> (Options -> String -> IO (Response a))
    -> OrchestrateIO (Response a)
api paths pairs o f = do
    opts <- views sessionOptions $ fromMaybe id o
    io . f opts =<< buildUrl paths pairs

api' :: [T.Text] -> [FormParam] -> Maybe (Options -> Options)
     -> (Options -> String -> IO (Response a))
     -> OrchestrateIO (Either Status (Response a))
api' paths pairs o f = do
    opts <- views sessionOptions $ fromMaybe id o
    url  <- buildUrl paths pairs
    io $ fmap Right (f opts url) `Ex.catch` handler
    where handler (StatusCodeException s _ _) = return $ Left s
          handler e                           = throwIO e

envSession :: IO Session
envSession = do
    key <- T.pack <$> getEnv "ORCHESTRATE_API"
    return $ def & sessionKey .~ key
                 & over sessionOptions (withAuth key)

rot :: (a -> b -> c -> d) -> c -> a -> b -> d
rot f c a b = f a b c

ifMatch :: IfMatch -> Options -> Options
ifMatch (Just r) = set (header "If-Match") [E.encodeUtf8 r]
ifMatch Nothing  = id

ifMatch' :: IfMatch' -> Options -> Options
ifMatch' (IfMatch r)     = withHeader "If-Match"      r
ifMatch' (IfNoneMatch r) = withHeader "If-None-Match" r
ifMatch' NoMatch         = id

withHeader :: CI BS.ByteString -> T.Text -> Options -> Options
withHeader h r = set (header h) [E.encodeUtf8 r]

-- locationKey :: (T.Text -> T.Text) -> T.Text -> T.Text -> T.Text
locationKey :: Prism' T.Text T.Text
locationKey = prism' (mappend "//") ((`atMay` 3) . T.split (=='/'))

locationRef :: Prism' T.Text T.Text
locationRef = prism' (mappend "////") ((`atMay` 5) . T.split (=='/'))

getLocation :: Response a -> T.Text
getLocation = E.decodeUtf8 . view (responseHeader "Location")

rangeStart :: FormValue a => BS.ByteString -> RangeEnd a -> Maybe FormParam
rangeStart k (Inclusive r) = Just $ ("start" <> k) := r
rangeStart k (Exclusive r) = Just $ ("after" <> k) := r
rangeStart _ Open          = Nothing

rangeEnd :: FormValue a => BS.ByteString -> RangeEnd a -> Maybe FormParam
rangeEnd k (Inclusive r) = Just $ ("end"    <> k) := r
rangeEnd k (Exclusive r) = Just $ ("before" <> k) := r
rangeEnd _ Open          = Nothing

tshow :: Show a => a -> T.Text
tshow = T.pack . show
