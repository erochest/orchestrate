{-# LANGUAGE OverloadedStrings #-}


module Database.Orchestrate.Graph
    ( RelKind
    , RelList

    , getRel
    , createRel
    , deleteRel
    ) where


import qualified Data.Text                  as T

import           Database.Orchestrate.Types


type RelKind = T.Text

getRel :: OrchestrateData a
       => a -> RelKind -> [RelKind] -> OrchestrateT m (RelList a b)
getRel = undefined

createRel :: (OrchestrateData a, OrchestrateData b)
          => a -> RelKind -> b -> OrchestrateT m ()
createRel = undefined

deleteRel :: (OrchestrateData a, OrchestrateData b)
          => a -> RelKind -> b -> OrchestrateT m ()
deleteRel = undefined

type RelList a b = ResultList (ResultItem Path b)
