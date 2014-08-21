{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}


{-|
This implements the <http://orchestrate.io/api/keyvalue Key/Value> API.

Generally, the data stored knows about its own key using via the
'OrchestrateData' class instance defined for it.
-}


-- TODO: exchange *V for *KV (should reflect the parameters required).

module Database.Orchestrate.KeyValue
    (
    -- * Data Types
      KVList

    -- * API Functions
    -- ** Accessing Data
    , lookup
    , listVals
    -- ** Adding and Updating Data
    , putV
    , putKV
    , postV
    -- ** Deleting Data
    , deleteV
    , deleteKV
    , purgeV
    , purgeKV
    ) where


import           Control.Applicative
import           Control.Arrow
import           Control.Error
import           Control.Lens
import           Control.Monad              (join, void)
import           Data.Aeson
import qualified Data.Text                  as T
import           Network.Wreq
import           Prelude                    hiding (lookup)

import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils


-- | This retrieves a value from a collection.
--
-- > lookup "contacts" "mom"
lookup :: FromJSON v => Collection -> Key -> OrchestrateIO (Maybe v)
lookup c k =
    join . fmap (decode . (^.responseBody)) <$> api404 [] [c, k] [] getWith

-- | This inserts data into the database or updates existing data using
-- a key generated by the 'OrchestrateData' instance.
--
-- > putV data NoMatch
putV :: OrchestrateData v
     => v                          -- ^ The data to store in the database.
     -> IfMatch                    -- ^ If specified, this operation only succeeds
                                   -- if the ref specified matches the
                                   -- currently stored ref for this data.
     -> OrchestrateIO Location     -- ^ Returns the location of the data.
putV v = putKV (dataKey v) v

-- | This inserts data into the database or updates data in the database.
-- This overrides the key provided by the data type's 'OrchestrateData'
-- instance. However, it still requires an implementation of that data type
-- for the collection name.
--
-- > putKV "key" data NoMatch
putKV :: OrchestrateData v
      => Key                     -- ^ The key to store the data under.
      -> v                       -- ^ The data to store.
      -> IfMatch                 -- ^ If specified, this operation only succeeds
                                 -- if the ref specified matches the currently
                                 -- stored ref for this data.
      -> OrchestrateIO Location  -- ^ Returns the location of the data.
putKV k v m =
    getLocation <$> api (ifMatch m) [tableName v, k] [] (rot putWith v')
    where v' = toJSON v

-- | This inserts data in the database, generating a new database key for
-- it.
--
-- > postV data
postV :: OrchestrateData v
      => v                                      -- ^ The data to store.
      -> OrchestrateIO (Location, Maybe Key)    -- ^ The 'Location' and key for the data.
postV v =   (id &&& firstOf locationKey) . getLocation
        <$> api [] [tableName v] [] (rot postWith (toJSON v))

-- | This removes data from the database.
--
-- > deleteV data Nothing
deleteV :: OrchestrateData v
         => v                   -- ^ The data to remove.
         -> IfMatch'            -- ^ If given, this operation only succeeds
                                -- if the ref specified matches the currently
                                -- stored ref for this data.
         -> OrchestrateIO ()
deleteV v = deleteKV (dataKey v) v

-- | This removes data from the database.
--
-- > deleteKV "key" data Nothing
deleteKV :: OrchestrateData v
         => Key                  -- ^ The key the data is stored under.
         -> v                    -- ^ The data to remove.
         -> IfMatch'             -- ^ If given, this operation only succeeds
                                 -- if the ref specified matches the
                                 -- currently stored ref for this data.
         -> OrchestrateIO ()
deleteKV k v m = void $ apiCheck (ifMatch' m) [tableName v, k] [] deleteWith

-- | This purges data from the database. Purging not only removes the data,
-- but also all history and secondary items for it.
--
-- > purgeV data Nothing
purgeV :: OrchestrateData v
       => v                    -- ^ The data to remove.
       -> IfMatch'             -- ^ If given, this operation only succeeds
                               -- if the ref specified matches the
                               -- currently stored ref for this data.
       -> OrchestrateIO ()
purgeV v = purgeKV (dataKey v) v

-- | This purges data from the database. Purging not only removes the data,
-- but also all history and secondary items for it.
--
-- > purgeKV "key" data Nothing
purgeKV :: OrchestrateData v
        => Key                   -- ^ The key the data is stored under.
        -> v                     -- ^ The data to remove.
        -> IfMatch'              -- ^ If given, this operation only succeeds
                                 -- if the ref specified matches the
                                 -- currently stored ref for this data.
        -> OrchestrateIO ()
purgeKV k v m =
    void $ apiCheck (ifMatch' m) [tableName v, k]
                    ["purge" := ("true" :: T.Text)] deleteWith

-- | This lists all the data in the collection within the range given.
--
-- > listVals "coll-name" Nothing (Open, Open)
listVals :: FromJSON v
         => Collection                -- ^ The collection to list data from.
         -> Maybe Int                 -- ^ The maximum number of items to retrieve.
         -> Range Key                 -- ^ The range of keys to query.
         -> OrchestrateIO (KVList v)  -- ^ Returns a collection of data.
listVals c limit (start, end) = apiCheckDecode [] [c] ps getWith
    where ps = catMaybes [ ("limit" :=) <$> limit
                         , rangeStart "Key" start
                         , rangeEnd   "Key" end
                         ]

-- | A list of data returned by 'listV'.
--
-- [@v@] The type of the data contained in the list.
type KVList v = ResultList (ResultItem Path v)
