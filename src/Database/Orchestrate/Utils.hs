{-# LANGUAGE FlexibleContexts  #-}
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
    , api404
    , apiCheck
    , apiCheckDecode
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
import           Control.Monad.Error.Class
import           Data.Aeson
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Builder      as B
import qualified Data.ByteString.Lazy         as BSL
import           Data.Default
import qualified Data.List                    as L
import           Data.Monoid
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as E
import           Network.HTTP.Client          hiding (responseBody)
import           Network.HTTP.Types
import           Network.Wreq
import           Network.Wreq.Types           hiding (auth)
import           System.Environment

import           Database.Orchestrate.Network
import           Database.Orchestrate.Types


ping :: OrchestrateIO ()
ping = checkResponse =<< api [] [] [] headWith

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

hPair :: Options -> Header -> Options
hPair o (k, v) = o & header k .~ [v]

api :: RequestHeaders -> [T.Text] -> [FormParam]
    -> (Options -> String -> IO (Response a))
    -> OrchestrateIO (Response a)
api hdrs paths pairs f = do
    opts <- views sessionOptions $ flip (L.foldl' hPair) hdrs
    io . f opts =<< buildUrl paths pairs

api' :: RequestHeaders -> [T.Text] -> [FormParam]
     -> (Options -> String -> IO (Response a))
     -> OrchestrateIO (Either Status (Response a))
api' hdrs paths pairs f = do
    opts <- views sessionOptions $ flip (L.foldl' hPair) hdrs
    url  <- buildUrl paths pairs
    io $ fmap Right (f opts url) `Ex.catch` handler
    where handler (StatusCodeException s _ _) = return $ Left s
          handler e                           = throwIO e

apiCheck :: RequestHeaders -> [T.Text] -> [FormParam]
         -> (Options -> String -> IO (Response a))
         -> OrchestrateIO (Response a)
apiCheck rh rpath rparams handler = do
    r <- api rh rpath rparams handler
    checkResponse r
    return r

apiCheckDecode :: FromJSON a
               => RequestHeaders -> [T.Text] -> [FormParam]
               -> (Options -> String -> IO (Response BSL.ByteString))
               -> OrchestrateIO a
apiCheckDecode rh rpath rparams handler =
        apiCheck rh rpath rparams handler
    >>= orchestrateEither . fmapL errex . eitherDecode . (^. responseBody)
    where errex = Ex.SomeException . Ex.ErrorCall

api404 :: RequestHeaders -> [T.Text] -> [FormParam]
       -> (Options -> String -> IO (Response a))
       -> OrchestrateIO (Maybe (Response a))
api404 hdrs pths parms f = do
    s  <- ask
    er <- liftIO $ Ex.catchJust
        Ex.fromException
        (fmapL filterStatusCode <$> runO' (api hdrs pths parms f) s)
        handler
    case er of
        Right r -> checkResponse r >> return (Just r)
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

envSession :: IO Session
envSession = do
    key <- T.pack <$> getEnv "ORCHESTRATE_API"
    return $ def & sessionKey .~ key
                 & over sessionOptions (withAuth key)

rot :: (a -> b -> c -> d) -> c -> a -> b -> d
rot f c a b = f a b c

ifMatch :: IfMatch -> [Header]
ifMatch (Just r) = [("If-Match", E.encodeUtf8 r)]
ifMatch Nothing  = []

ifMatch' :: IfMatch' -> [Header]
ifMatch' (IfMatch r)     = [("If-Match",      E.encodeUtf8 r)]
ifMatch' (IfNoneMatch r) = [("If-None-Match", E.encodeUtf8 r)]
ifMatch' NoMatch         = []

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
