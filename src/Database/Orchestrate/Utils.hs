{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wall #-}

module Database.Orchestrate.Utils
    ( ping
    , baseUrl
    , authOptions
    , api
    , api'
    , envSession
    , rot
    , ifMatch
    , ifMatch'
    , locationKey
    , locationRef
    , rangeParams
    , rangeStart
    , rangeEnd
    , tshow
    ) where


import           Control.Applicative
import           Control.Error
import           Control.Exception            as Ex
import           Control.Lens
import qualified Data.ByteString              as BS
import           Data.CaseInsensitive
import           Data.Default
import           Data.Monoid
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as E
import           Network.HTTP.Client
import           Network.Wreq
import           System.Environment

import           Database.Orchestrate.Network
import           Database.Orchestrate.Types


ping :: OrchestrateIO ()
ping = checkResponse =<< api [] [] Nothing headWith

baseUrl :: Monad m => OrchestrateT m T.Text
baseUrl = do
    Session{..} <- ask
    return $ mconcat [_sessionURL, "/v", tshow _sessionVersion]

authOptions :: Monad m => OrchestrateT m Options
authOptions = do
    key <- E.encodeUtf8 <$> asks _sessionKey
    return $ defaults & auth .~ basicAuth key ""
                      & header "Content-Type" .~ ["application/json"]
                      & header "Accept"       .~ ["application/json"]

addp :: [T.Text] -> T.Text -> T.Text
addp [] url = url
addp p  url = mconcat [url, "?", T.intercalate "&" p]

buildUrl :: [T.Text] -> [T.Text] -> T.Text -> String
buildUrl pr pt = T.unpack . addp pr . T.intercalate "/" . (:pt)

addAuthOptions :: Monad m => Maybe (Options -> Options) -> OrchestrateT m Options
addAuthOptions mf = fromMaybe id mf <$> authOptions

api :: [T.Text] -> [T.Text] -> Maybe (Options -> Options)
    -> (Options -> String -> IO (Response a))
    -> OrchestrateIO (Response a)
api paths pairs o f = do
    opts <- addAuthOptions o
    io . f opts =<< fmap (buildUrl pairs paths) baseUrl

api' :: [T.Text] -> [T.Text] -> Maybe (Options -> Options)
     -> (Options -> String -> IO (Response a))
     -> OrchestrateIO (Either Status (Response a))
api' paths pairs o f = do
    opts <- addAuthOptions o
    url  <- buildUrl pairs paths <$> baseUrl
    io $ fmap Right (f opts url) `Ex.catch` handler
    where handler (StatusCodeException s _ _) = return $ Left s
          handler e                           = throwIO e

envSession :: IO Session
envSession = do
    key <- T.pack <$> getEnv "ORCHESTRATE_API"
    return $ def & sessionKey .~ key

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

rangeParams :: Show a => T.Text -> Range a -> [T.Text]
rangeParams k (start, end) = catMaybes [ rangeStart k start
                                       , rangeEnd   k end
                                       ]

rangeStart :: Show a => T.Text -> RangeEnd a -> Maybe T.Text
rangeStart k (Inclusive r) = Just $ mconcat ["start", k, "=", tshow r]
rangeStart k (Exclusive r) = Just $ mconcat ["after", k, "=", tshow r]
rangeStart _ Open          = Nothing

rangeEnd :: Show a => T.Text -> RangeEnd a -> Maybe T.Text
rangeEnd k (Inclusive r) = Just $ mconcat ["end",    k, "=", tshow r]
rangeEnd k (Exclusive r) = Just $ mconcat ["before", k, "=", tshow r]
rangeEnd _ Open          = Nothing

tshow :: Show a => a -> T.Text
tshow = T.pack . show
