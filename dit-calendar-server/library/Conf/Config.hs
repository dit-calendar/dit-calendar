{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Conf.Config
    ( AppConfig(..)
    , CorsConfig(..)
    , ServerConfig(..)
    , customHappstackServerConf
    , serverConfigHostUri
    ) where

import           Conferer
import           Data.Text
import           Happstack.Server       (nullConf)
import           Happstack.Server.Types (Conf, port)

import           GHC.Generics           (Generic)

data AppConfig = AppConfig
    { appConfigServer :: ServerConfig
    , appConfigCors   :: CorsConfig
    } deriving (Show, Generic)

data CorsConfig = CorsConfig
    { corsConfigAllowOrigin      :: String
    , corsConfigAllowCredentials :: Bool
    } deriving (Show, Generic)

data ServerConfig = ServerConfig
    { serverConfigHost    :: String
    , serverConfigPort    :: Int
    , serverConfigProtocol :: String
    } deriving (Show, Generic)

serverConfigHostUri :: ServerConfig -> String
serverConfigHostUri c = serverConfigProtocol c ++ "://" ++ serverConfigHost c ++ ":" ++ show (serverConfigPort c)

instance FromConfig AppConfig
instance FromConfig ServerConfig
instance FromConfig CorsConfig

instance DefaultConfig AppConfig where
    configDef = AppConfig {
        appConfigServer = configDef,
        appConfigCors = configDef
    }

instance DefaultConfig ServerConfig where
    configDef = ServerConfig {
        serverConfigHost = "localhost"
        , serverConfigPort = 8080
        , serverConfigProtocol = "http"
    }

instance DefaultConfig CorsConfig where
    configDef = CorsConfig{
        corsConfigAllowOrigin = "http://localhost:8000"
        , corsConfigAllowCredentials = True
    }


customHappstackServerConf :: ServerConfig -> Conf
customHappstackServerConf netConf =
    nullConf { port = serverConfigPort netConf }
