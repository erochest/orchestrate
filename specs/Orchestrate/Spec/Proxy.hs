{-# LANGUAGE OverloadedStrings #-}


module Orchestrate.Spec.Proxy where


import           Data.Aeson
import           Data.Default
import           Filesystem
import           Filesystem.Path.CurrentOS
import Network.HTTP.Types
import qualified Data.ByteString.Lazy as BS

import           Database.Orchestrate.Types


data ProxySession = ProxySession
                  { restSession   :: !RestSession
                  , proxyCacheDir :: !FilePath
                  } deriving (Show)

data RequestData = RequestData
                 { rqMethod  :: !Method
                 , rqHeaders :: !RequestHeaders
                 , rqPath    :: !URLPath
                 , rqParam   :: ![FormParam]
                 } deriving (Show, Eq)

-- TODO, {From,To}JSON

data ResponseData = ResponseData
                  { respHeaders :: !ResponseHeaders
                  , respBody    :: !BS.ByteString
                  , respLink    :: ![(BS.ByteString, BS.ByteString, Link)]
                  , respCookies :: ![Cookie]
                  , respStatus  :: !Status
                  } deriving (Show)

-- TODO, {From,To}JSON

instance Default ProxySession where
    def = ProxySession def "."

checkCache :: ProxySession -> Request a -> IO (Maybe (Request a))
checkCache = undefined

insertCache :: ProxySession -> Request a -> IO ()
insertCache = undefined

instance RestClient ProxySession OrchestrateIO where
    restGet hdrs pth prm s = undefined
    restDelete hdrs pth prm s = undefined
    restPut hdrs pth d s = undefined
    restPost hdrs pth d s = undefined
