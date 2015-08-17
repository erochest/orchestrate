{-# LANGUAGE OverloadedStrings #-}


{- |
This module implements some network-oriented utility functions.
-}

module Database.Orchestrate.Network
    ( checkResponse
    , checkStatusCode
    ) where


import qualified Control.Exception          as Ex
import           Control.Lens
import           Network.Wreq

import           Database.Orchestrate.Types


-- | This takes a response and checks that the status code passes.
--
-- If the HTTP status code is not OK, this returns an error in the
-- 'OrchestrateT' monad.
checkResponse :: Monad m => Response a -> OrchestrateT m ()
checkResponse = checkStatusCode . view (responseStatus . statusCode)

-- | This checks the status code. Currently 200 and 204 are OK. Everything
-- else is bad. Bad codes throw an exception in 'OrchestrateT'.
checkStatusCode :: Monad m => Int -> OrchestrateT m ()
checkStatusCode 200 = return ()
checkStatusCode 201 = return ()
checkStatusCode 202 = return ()
checkStatusCode 204 = return ()
checkStatusCode rc  = throwError
                    . Ex.SomeException
                    . Ex.ErrorCall
                    $ "Invalid response: " ++ show rc
