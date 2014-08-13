{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Database.Orchestrate.Ref
    ( TombstoneItem(..)
    , _TombstoneItem
    , _LiveItem
    , livePath
    , liveTime
    , liveValue
    , tombstonePath
    , tombstoneTime

    , getRef
    , listRefs
    ) where


import           Control.Applicative
import           Control.Error
import qualified Control.Exception as Ex
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types           (Parser)
import qualified Data.HashMap.Strict        as M
import qualified Data.Text as T
import           Network.Wreq

import           Database.Orchestrate.Network
import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils


getRef :: FromJSON r => Collection -> Key -> Ref -> OrchestrateIO (Maybe r)
getRef c k r =
    join . fmap (decode . (^.responseBody)) <$> api404 [] [c, k, "refs", r] [] getWith

listRefs :: FromJSON v
         => Collection -> Key -> Maybe Int -> Maybe Int -> Bool
         -> OrchestrateIO (ResultList (TombstoneItem v))
listRefs c k limit offset values = do
    r <- api [] [c, k, "refs"] parms getWith
    checkResponse r
    orchestrateEither . fmapL errex . eitherDecode $ r ^. responseBody
    where errex = Ex.SomeException . Ex.ErrorCall
          ifm :: Bool -> a -> Maybe a
          ifm True  = Just
          ifm False = const Nothing
          parms = catMaybes [ ifm values $ "values" := ("true" :: T.Text)
                            , ("limit"  :=) . show <$> limit
                            , ("offset" :=) . show <$> offset
                            ]

data TombstoneItem v = TombstoneItem { _tombstonePath :: !Path
                                     , _tombstoneTime :: !Timestamp
                                     }
                     | LiveItem      { _livePath  :: !Path
                                     , _liveValue :: !(Maybe v)
                                     , _liveTime  :: !Timestamp
                                     }
                     deriving (Show)
$(makeLenses ''TombstoneItem)

_TombstoneItem :: Prism' (TombstoneItem v) (TombstoneItem v)
_TombstoneItem = prism' id $ \i ->
    case i of
        TombstoneItem{} -> Just i
        LiveItem{}      -> Nothing

_LiveItem :: Prism' (TombstoneItem v) (TombstoneItem v)
_LiveItem = prism' id $ \i ->
    case i of
        TombstoneItem{} -> Nothing
        LiveItem{}      -> Just i

emptyObject :: FromJSON v => Maybe Value -> Parser (Maybe v)
emptyObject (Just o@(Object m)) | M.null m  = return Nothing
                                | otherwise = parseJSON o
emptyObject _                               = return Nothing

instance FromJSON v => FromJSON (TombstoneItem v) where
    parseJSON (Object o) = do
        let path    = o .:  "path"
            reftime = o .:  "reftime"
        (Object p) <- o .:  "path"
        tombstone  <- p .:? "tombstone"
        case tombstone of
            Just (Bool True) -> TombstoneItem <$> path <*> reftime
            _                -> LiveItem <$> path <*> (emptyObject =<< o .:? "value") <*> reftime
    parseJSON _          = mzero
