{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Database.Orchestrate.Utils
    ( ping
    , baseUrl
    , authOptions
    , envSession
    ) where


import           Control.Applicative
import           Control.Lens
import           Data.Default
import           Data.Monoid
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as E
import           Network.Wreq
import           System.Environment

import           Database.Orchestrate.Network
import           Database.Orchestrate.Types


ping :: OrchestrateIO ()
ping = do
    Session{..} <- ask
    r <- io
        . headWith (defaults & auth .~ basicAuth (E.encodeUtf8 sessionKey) "")
        . T.unpack
        $ mconcat [sessionURL, "/v", T.pack $ show sessionVersion]
    checkResponse r

baseUrl :: Monad m => OrchestrateT m T.Text
baseUrl = do
    Session{..} <- ask
    return $ mconcat [sessionURL, "/v", T.pack (show sessionVersion)]

authOptions :: Monad m => OrchestrateT m Options
authOptions = do
    key <- asks sessionKey
    return $ defaults & auth .~ basicAuth (E.encodeUtf8 key) ""

envSession :: IO Session
envSession = do
    key <- T.pack <$> getEnv "ORCHESTRATE_API"
    return $ def { sessionKey = key }
