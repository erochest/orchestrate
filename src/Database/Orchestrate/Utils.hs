{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wall #-}


module Database.Orchestrate.Utils
    (
    -- * Executing 'OrchestrateIO' Actions
      runO
    , runO'

    -- * API Infrastructure
    , api
    , api'
    , api404
    , apiCheck
    , apiCheckDecode

    -- * API Functions
    , ping

    -- * Data Type Helpers
    -- ** Session Utilities
    , baseUrl
    , buildUrl
    , withAuth'
    , withAuth
    , envSession

    -- ** Match Utilities
    , ifMatch
    , ifMatch'

    -- ** 'Location' Prisms and Functions
    , locationCollection
    , locationKey
    , locationRef
    , locationType
    , locationTimestamp
    , locationOrdinal
    , getLocation

    -- ** Range Utilities
    , rangeStart
    , rangeEnd

    -- * General Utilities
    , rot
    , tshow
    , initTail
    ) where


import           Control.Applicative
import           Control.Error
import           Control.Exception            as Ex
import           Control.Lens
import           Control.Monad.Error.Class
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Builder      as B
import qualified Data.ByteString.Lazy         as BSL
import           Data.Default
import qualified Data.List                    as L
import           Data.Monoid
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as E
import qualified Data.Text.Read               as TR
import           Network.HTTP.Client          hiding (responseBody)
import           Network.HTTP.Types
import           Network.Wreq
import           Network.Wreq.Types           hiding (auth, headers, params)
import           System.Environment

import           Database.Orchestrate.Network
import           Database.Orchestrate.Types


-- | Pings the Orchestrate API.
ping :: OrchestrateIO ()
ping = checkResponse =<< api [] [] [] headWith

-- | Run an 'OrchestrateT' action with a 'Session' that does /not/ include
-- authentication. This function will add proper authentication before
-- running the action.
runO :: Monad m => OrchestrateT m a -> Session -> m (Either Ex.SomeException a)
runO m s = runO' m $ over sessionOptions (withAuth $ s ^. sessionKey) s

-- | Run an 'OrchestrateT' action with a 'Session' that already includes
-- authentication.
--
-- This is the most minimal handler.
runO' :: Monad m => OrchestrateT m a -> Session -> m (Either Ex.SomeException a)
runO' m = runReaderT (runEitherT $ runOrchestrate m)

-- | Create the base Orchestrate API URL given the current 'Session'.
baseUrl :: Monad m => OrchestrateT m T.Text
baseUrl = do
    Session{..} <- ask
    return $ mconcat [_sessionURL, "/v", tshow _sessionVersion]

-- | Adds the API key to the default 'Options'.
withAuth' :: APIKey -> Options
withAuth' key = withAuth key defaults

-- | Adds the API key to a set of wreq 'Options'.
withAuth :: APIKey -> Options -> Options
withAuth key o = o & auth .~ basicAuth (E.encodeUtf8 key) ""

-- | Builds a URL from its assembled parts.
buildUrl :: Monad m
         => [T.Text]                -- ^ The parts of the URL path. These are joined by @/@.
         -> OrchestrateT m String   -- ^ Returns the URL as a 'String'.
buildUrl paths = do
    Session{_sessionURL, _sessionVersion} <- ask
    return . T.unpack
           . E.decodeUtf8
           . BSL.toStrict
           . B.toLazyByteString
           . mconcat
           . mappend [ textb _sessionURL
                     , "/v", B.intDec _sessionVersion
                     , "/"
                     ]
           . L.intersperse "/"
           $ map textb paths
    where textb = B.byteString . E.encodeUtf8

augmentOptions :: RequestHeaders -> [FormParam] -> Options -> Options
augmentOptions hdrs pairs o = o & headers .~ hdrs
                                & params  .~ map pair pairs
    where bstext        = E.decodeUtf8
          pair (k := v) = (bstext k, bstext $ renderFormValue v)

-- | This assembles and performs an API call.
api :: RequestHeaders               -- ^ Additional HTTP headers.
    -> [T.Text]                     -- ^ The parts of the URL path.
    -> [FormParam]                  -- ^ The form parameters.
    -> RestCall a                   -- ^ The wreq function to make the call.
    -> OrchestrateIO (Response a)   -- ^ Returns the call's response.
api hdrs paths pairs f = do
    opts <- views sessionOptions $ augmentOptions hdrs pairs
    io . f opts =<< buildUrl paths

-- | This assembles and peforms an API call, lifting any status code errors
-- out of the monad and returning them in an explicit 'Either'.
api' :: RequestHeaders              -- ^ Additional HTTP headers.
     -> [T.Text]                    -- ^ The parts of the URL path.
     -> [FormParam]                 -- ^ The form parameters.
     -> RestCall a                  -- ^ The wreq function to make the call.
     -> OrchestrateIO (Either Status (Response a))
                                    -- ^ Returns either the error status or
                                    -- the response.
api' hdrs paths pairs f = do
    opts <- views sessionOptions $ augmentOptions hdrs pairs
    url  <- buildUrl paths
    io $ fmap Right (f opts url) `Ex.catch` handler
    where handler (StatusCodeException s _ _) = return $ Left s
          handler e                           = throwIO e

-- | This assembles and performs an API call and checks that the status
-- passes 'checkResponse'.
apiCheck :: RequestHeaders              -- ^ Additional HTTP headers.
         -> [T.Text]                    -- ^ The parts of the URL path.
         -> [FormParam]                 -- ^ The form parameters.
         -> RestCall a                  -- ^ The wreq function to make the call.
         -> OrchestrateIO (Response a)  -- ^ Returns the verified response.
apiCheck rh rpath rparams handler = do
    r <- api rh rpath rparams handler
    checkResponse r
    return r

-- | This assembles and performs an API call. Afterward it checks the
-- status code and decodes the JSON response.
apiCheckDecode :: FromJSON a
               => RequestHeaders            -- ^ Additional HTTP headers.
               -> [T.Text]                  -- ^ The parts of the URL path.
               -> [FormParam]               -- ^ The form parameters.
               -> RestCall BSL.ByteString   -- ^ The wreq function to make the call.
               -> OrchestrateIO a           -- ^ Returns the verified, decoded response.
apiCheckDecode rh rpath rparams handler =
        apiCheck rh rpath rparams handler
    >>= orchestrateEither . fmapL errex . eitherDecode . (^. responseBody)
    where errex = Ex.SomeException . Ex.ErrorCall

-- | This assembles and performs an API call. It returns 'Nothing' if the
-- call returns a 404 status code.
api404 :: Show a
       => RequestHeaders                    -- ^ Additional HTTP headers.
       -> [T.Text]                          -- ^ The parts of the URL path.
       -> [FormParam]                       -- ^ The form parameters.
       -> RestCall a                        -- ^ The wreq function to make the call.
       -> OrchestrateIO (Maybe (Response a))    -- ^ Returns maybe the response.
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

-- | This returns the 'Session' with the API key taken from the
-- @ORCHESTRATE_API@ environment variable.
--
-- The value of 'sessionOptions' will include authentication.
envSession :: IO Session
envSession = do
    key <- T.pack <$> getEnv "ORCHESTRATE_API"
    return $ def & sessionKey .~ key
                 & over sessionOptions (withAuth key)

-- | This takes a three-parameter function and rotates the parameters to
-- return a function that takes the third parameter first.
rot :: (a -> b -> c -> d) -> c -> a -> b -> d
rot f c a b = f a b c

-- | Takes an 'IfMatch'' and returns a list of request headers.
ifMatch' :: IfMatch' -> [Header]
ifMatch' (Just r) = [("If-Match", E.encodeUtf8 r)]
ifMatch' Nothing  = []

-- | Takes an 'IfMatch' and returns a list of request headers.
ifMatch :: IfMatch -> [Header]
ifMatch (IfMatch r)     = [("If-Match",      E.encodeUtf8 r)]
ifMatch (IfNoneMatch r) = [("If-None-Match", E.encodeUtf8 r)]
ifMatch NoMatch         = []

loc :: Int -> Prism' T.Text T.Text
loc n = prism' (mappend $ T.replicate (n - 1) "/")
               ((`atMay` n) . T.split (=='/'))

locint :: (Integral i, Show i) => Int -> Prism' T.Text i
locint n = prism' (mappend prefix . T.pack . show)
                  ( join
                  . fmap (hush . fmap fst . TR.decimal)
                  . (`atMay` n)
                  . T.split (=='/'))
    where prefix = T.replicate (n - 1) "/"

-- | A prism over the collection part of a 'Location' URL.
locationCollection :: Prism' T.Text T.Text
locationCollection = loc 2

-- | A prism over the key part of a 'Location' URL.
locationKey :: Prism' T.Text T.Text
locationKey = loc 3

-- | A prism over the ref part of a 'Location' URL.
locationRef :: Prism' T.Text T.Text
locationRef = loc 5

-- | A prism over the type part of a 'Location' URL from an event
-- operation.
locationType :: Prism' T.Text T.Text
locationType = loc 5

-- | A prism over the timestamp part of a 'Location' URL from an event
-- operation.
locationTimestamp :: Prism' T.Text Integer
locationTimestamp = locint 6

-- | A prism over the ordinal part of a 'Location' URL from an event
-- operation.
locationOrdinal :: Prism' T.Text Int
locationOrdinal = locint 7

-- | Retrieves the 'Location' from a response's headers.
getLocation :: Response a -> T.Text
getLocation = E.decodeUtf8 . view (responseHeader "Location")

-- | Given a starting 'RangeEnd', return the form parameter.
rangeStart :: FormValue a
           => BS.ByteString     -- ^ The suffix for the form parameter.
           -> RangeEnd a        -- ^ The 'RangeEnd'.
           -> Maybe FormParam   -- ^ Returns the form parameter.
rangeStart k (Inclusive r) = Just $ ("start" <> k) := r
rangeStart k (Exclusive r) = Just $ ("after" <> k) := r
rangeStart _ Open          = Nothing

-- | Given an ending 'RangeEnd', return the form parameter.
rangeEnd :: FormValue a
         => BS.ByteString       -- ^ The suffix for the form parameter.
         -> RangeEnd a          -- ^ The 'RangeEnd'.
         -> Maybe FormParam     -- ^ Returns the form parameter.
rangeEnd k (Inclusive r) = Just $ ("end"    <> k) := r
rangeEnd k (Exclusive r) = Just $ ("before" <> k) := r
rangeEnd _ Open          = Nothing

-- | Show data as 'T.Text'.
tshow :: Show a => a -> T.Text
tshow = T.pack . show

-- | Returns both the last item in a list and the list's 'init'.
--
-- If the list is empty, it will return @(Nothing, [])@. Otherwise, it's an
-- optimized call to @(lastMay &&& initSafe)@.
initTail :: [a] -> (Maybe a, [a])
initTail []     = (Nothing, [])
initTail [x]    = (Just x,  [])
initTail (x:xs) = (x:) <$> initTail xs
