{-# LANGUAGE OverloadedStrings #-}


module Database.Orchestrate.Graph
    ( RelKind
    , RelList

    , getRel
    , createRel
    , deleteRel
    ) where


import           Control.Monad
import           Data.Aeson
import qualified Data.Text                  as T
import           Network.Wreq

import           Database.Orchestrate.Types
import           Database.Orchestrate.Utils


type RelKind = T.Text

getRel :: (OrchestrateData a, FromJSON b)
       => a -> RelKind -> [RelKind] -> OrchestrateIO (RelList a b)
getRel from rel rels = apiCheckDecode [] url [] getWith
    where url = tableName from : dataKey from : "relations" : rel : rels

createRel :: (OrchestrateData a, OrchestrateData b)
          => a -> RelKind -> b -> OrchestrateIO ()
createRel from rel to = void $ apiCheck [] url [] $ \o s -> putWith o s Null
    where url = [ tableName from , dataKey from
                , "relation", rel
                , tableName to, dataKey to
                ]

deleteRel :: (OrchestrateData a, OrchestrateData b)
          => a -> RelKind -> b -> OrchestrateIO ()
deleteRel from rel to =
    void $ apiCheck [] url ["purge" := ("true" :: T.Text)] deleteWith
    where url = [ tableName from, dataKey from
                , "relation", rel
                , tableName to, dataKey to
                ]

type RelList a b = ResultList (ResultItem Path b)
