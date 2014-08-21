{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{- |
This module implements the <http://orchestrate.io/api/keyvalue Orchestrate Refs API>.

Refs represent the immutable values that have been associated with a key.
-}


module Database.Orchestrate.Ref
    (
    -- * API Functions
      getRef
    , listRefs

    -- * Types
    , TombstoneItem(..)

    -- ** Type Lenses and Prisms
    , _TombstoneItem
    , _LiveItem
    , livePath
    , liveTime
    , liveValue
    , tombstonePath
    , tombstoneTime
    ) where


import           Control.Applicative
import           Control.Error
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types           (Parser)
import qualified Data.HashMap.Strict        as M
import qualified Data.Text                  as T
import           Network.Wreq

import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils


-- | This retrieves a ref associated with some data.
--
-- > getRef "coll-name" "key" "43214321"
getRef :: FromJSON r => Collection -> Key -> Ref -> OrchestrateIO (Maybe r)
getRef c k r =
    join . fmap (decode . (^.responseBody)) <$> api404 [] [c, k, "refs", r] [] getWith

-- | This lists all the values that have been associated with a key in the
-- database. Values are returned last to first.
--
-- > listRefs "coll-name" "key" Nothing Nothing False
listRefs :: FromJSON v
         => Collection      -- ^ This is the collection for the data.
         -> Key             -- ^ This is the key for the data.
         -> Maybe Int       -- ^ The maximum number of items to return.
         -> Maybe Int       -- ^ The offset into the total sequence of values.
         -> Bool            -- ^ Should the values be returned also, or just the 'Path' data?
         -> OrchestrateIO (ResultList (TombstoneItem v))
                            -- ^ Returns the list of results.
listRefs c k limit offset values =
    apiCheckDecode [] [c, k, "refs"] parms getWith
    where ifm True  = Just
          ifm False = const Nothing
          parms = catMaybes [ ifm values $ "values" := ("true" :: T.Text)
                            , ("limit"  :=) . show <$> limit
                            , ("offset" :=) . show <$> offset
                            ]

-- | 'TombstoneItem' data represents the data values in the database.
data TombstoneItem v =
    -- | 'TombstoneItem' data are no longer alive. They are simply markers
    -- for deleted data.
      TombstoneItem { _tombstonePath :: !Path       -- ^ The path to the deleted data.
                    , _tombstoneTime :: !Timestamp  -- ^ The timestamp of this data.
                    }
    -- | 'LiveItem' data are still in the database.
    | LiveItem      { _livePath  :: !Path           -- ^ The path to the data.
                    , _liveValue :: !(Maybe v)      -- ^ If values are requested,
                                                    -- this will contain the data.
                    , _liveTime  :: !Timestamp      -- ^ The timestamp for the data.
                    }
                    deriving (Show)
$(makeLenses ''TombstoneItem)

-- | A 'Prism'' into data created with the 'TombstoneItem' constructor.
_TombstoneItem :: Prism' (TombstoneItem v) (TombstoneItem v)
_TombstoneItem = prism' id $ \i ->
    case i of
        TombstoneItem{} -> Just i
        LiveItem{}      -> Nothing

-- | A 'Prism'' into data created with the 'LiveItem' constructor.
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
