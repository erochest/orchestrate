{-# LANGUAGE OverloadedStrings #-}


module Database.Orchestrate.Network
    ( checkResponse
    , checkStatusCode
    ) where


import           Control.Lens
import           Data.Monoid
import qualified Data.Text                  as T
import           Network.Wreq

import           Database.Orchestrate.Types


checkResponse :: Monad m => Response a -> OrchestrateT m ()
checkResponse = checkStatusCode . view (responseStatus . statusCode)

checkStatusCode :: Monad m => Int -> OrchestrateT m ()
checkStatusCode 200 = return ()
checkStatusCode 204 = return ()
checkStatusCode rc  = throwError $ "Invalid response: " <> (T.pack . show) rc
