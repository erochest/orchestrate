{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Database.Orchestrate.Utils
    ( ping
    ) where


import           Control.Lens
import           Data.Monoid
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as E
import           Network.Wreq

import           Database.Orchestrate.Types


ping :: OrchestrateIO ()
ping = do
    Session{..} <- ask
    r <- fmap (view (responseStatus . statusCode))
        . io
        . headWith (defaults & auth .~ basicAuth (E.encodeUtf8 sessionKey) "")
        . T.unpack
        $ mconcat [sessionURL, "/v", T.pack $ show sessionVersion]
    if r == 200
        then return ()
        else throwError $ "Invalid response: " <> (T.pack . show) r
