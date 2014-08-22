{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{- |
This module implements the <http://orchestrate.io/api/keyvalue Orchestrate Refs API>.

Refs represent the immutable values that have been associated with a key.
-}


module Database.Orchestrate.Ref
    ( getRef
    , listRefs
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
