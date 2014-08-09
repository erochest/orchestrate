{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -Wall #-}


-- TODO: Clean this up some. Eeek.

module Database.Orchestrate.Types
    ( APIKey
    , Collection
    , Key
    , Ref
    , Timestamp
    , Location
    , IfMatch
    , IfMatch'(..)
    , Range
    , RangeEnd(..)
    , Limit
    , Offset

    , RestClient(..)

    , RestSession(..)
    , sessionURL
    , sessionKey
    , sessionVersion
    , sessionOptions

    , OrchestrateData(..)
    , OrchestrateT(..)
    , OrchestrateIO
    , Orchestrate
    , orchestrateEither

    , ask
    , asks

    , io

    , throwError
    , catchError

    , ResultList(..)
    , resultCount
    , resultList
    , resultPrev
    , resultNext

    , ResultItem(..)
    , itemPath
    , itemValue

    , Path(..)
    , itemCollection
    , itemKey
    , itemRef
    ) where


import           Control.Applicative
import           Control.Error
import qualified Control.Exception         as Ex
import           Control.Lens
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString.Builder   as B
import qualified Data.ByteString.Lazy      as BSL
import           Data.Default
import qualified Data.List                 as L
import           Data.Monoid
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as E
import           Network.HTTP.Types.Header
import           Network.Wreq
import           Network.Wreq.Types


type APIKey     = T.Text
type Collection = T.Text
type Key        = T.Text
type Ref        = T.Text
type Timestamp  = Integer
type Location   = T.Text
type IfMatch    = Maybe Ref
type Limit      = Int
type Offset     = Int

data IfMatch'   = IfMatch Ref
                | IfNoneMatch Ref
                | NoMatch
                deriving (Show)

type Range a    = (RangeEnd a, RangeEnd a)
data RangeEnd a = Inclusive a
                | Exclusive a
                | Open
                deriving (Show, Functor)

type URLPath   = [T.Text]

-- The primary point of this type class is allowing me to mock it for
-- testing. Thus, I've still relied heavily on the `wreq` types. I expect
-- that to be the primary implementation.
class RestClient s m where
    restGet    :: RequestHeaders -> URLPath -> [FormParam] -> s -> m (Response BSL.ByteString)
    restDelete :: RequestHeaders -> URLPath -> [FormParam] -> s -> m (Response ())
    restPut    :: Putable a  => RequestHeaders -> URLPath -> a -> s -> m (Response BSL.ByteString)
    restPost   :: Postable a => RequestHeaders -> URLPath -> a -> s -> m (Response BSL.ByteString)

data RestSession = RestSession
                 { _sessionURL     :: !T.Text
                 , _sessionKey     :: !APIKey
                 , _sessionVersion :: !Int
                 , _sessionOptions :: !Options
                 } deriving (Show)
$(makeLenses ''RestSession)

instance Default RestSession where
    def = RestSession "https://api.orchestrate.io" "" 0
        $ defaults & header "Content-Type" .~ ["application/json"]
                   & header "Accept"       .~ ["application/json"]

addHeaderPair :: Options -> Header -> Options
addHeaderPair o (k, v) = o & header k .~ [v]

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
    let paths' = L.foldr (jn "/") parms' $ map (B.byteString . E.encodeUtf8) paths
    return . T.unpack . E.decodeUtf8 . BSL.toStrict . B.toLazyByteString $ mconcat [ B.byteString $ E.encodeUtf8 url
                                                , "/v", B.intDec v
                                                , "/", paths'
                                                ]
    where jn j x y = x <> j <> y
          toPair (k := v) = (k, renderFormValue v)

api :: (Options -> String -> IO (Response a))
    -> RequestHeaders -> URLPath -> [FormParam] -> OrchestrateIO (Response a)
api f hdrs paths parms = do
    opts <- flip (L.foldl' addHeaderPair) hdrs <$> view sessionOptions
    url  <- buildUrl paths parms
    io $ f opts url

instance RestClient RestSession OrchestrateIO where
    restGet h pt pm _    = api getWith h pt pm
    restDelete h pt pm _ = api deleteWith h pt pm
    restPut h pt d _     = api (\o u -> putWith o u d) h pt []
    restPost h pt d _    = api (\o u -> postWith o u d) h pt []

class (ToJSON a, FromJSON a) => OrchestrateData a where
    tableName :: a -> T.Text
    dataKey   :: a -> T.Text

newtype OrchestrateT m a
    = OrchestrateT { runOrchestrate :: EitherT Ex.SomeException (ReaderT RestSession m) a }
    deriving (Functor, Applicative, Monad)

instance MonadTrans OrchestrateT where
    lift = OrchestrateT . lift . lift

instance MonadIO m => MonadIO (OrchestrateT m) where
    liftIO = OrchestrateT . liftIO . liftIO

instance Monad m => MonadReader RestSession (OrchestrateT m) where
    ask     = OrchestrateT . lift $ ask
    local f = OrchestrateT . local f . runOrchestrate

io :: MonadIO m => IO a -> OrchestrateT m a
io = OrchestrateT . syncIO

-- TODO: Need to define this for other monad classes.

instance Monad m => MonadError Ex.SomeException (OrchestrateT m) where
    throwError = OrchestrateT . EitherT . return . Left
    catchError a handler =   join
                         .   fmap (handler' handler)
                         .   lift
                         .   runReaderT (runEitherT $ runOrchestrate a)
                         =<< ask

handler' :: Monad m
         => (Ex.SomeException -> OrchestrateT m a) -> Either Ex.SomeException a
         -> OrchestrateT m a
handler' _ (Right v) = return v
handler' f (Left e)  = f e

orchestrateEither :: Monad m => Either Ex.SomeException a -> OrchestrateT m a
orchestrateEither = OrchestrateT . hoistEither

type Orchestrate   = OrchestrateT Identity
type OrchestrateIO = OrchestrateT IO

data Path = Path
          { _itemCollection :: !T.Text
          , _itemKey        :: !T.Text
          , _itemRef        :: !T.Text
          } deriving (Show)
$(makeLenses ''Path)

instance FromJSON Path where
    parseJSON (Object o) =   Path
                         <$> o .: "collection"
                         <*> o .: "key"
                         <*> o .: "ref"
    parseJSON _          =   mzero

data ResultList i = ResultList
                  { _resultCount :: !Int
                  , _resultList  :: ![i]
                  , _resultPrev  :: !(Maybe Location)
                  , _resultNext  :: !(Maybe Location)
                  } deriving (Show)
$(makeLenses ''ResultList)

instance FromJSON r => FromJSON (ResultList r) where
    parseJSON (Object o) =   ResultList
                         <$> o .:  "count"
                         <*> o .:  "results"
                         <*> o .:? "prev"
                         <*> o .:? "next"
    parseJSON _          =   mzero

data ResultItem p v = ResultItem
                    { _itemPath  :: !p
                    , _itemValue :: !v
                    } deriving (Show)
$(makeLenses ''ResultItem)

instance (FromJSON p, FromJSON v) => FromJSON (ResultItem p v) where
    parseJSON (Object o) =   ResultItem
                         <$> o .: "path"
                         <*> o .: "value"
    parseJSON _          =   mzero
