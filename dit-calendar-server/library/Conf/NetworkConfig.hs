{-# LANGUAGE OverloadedStrings #-}

module Conf.NetworkConfig (NetworkConfig(..), customHappstackServerConf, networkConfigParser) where

import           Data.Ini.Config

import           Happstack.Server       (nullConf)
import           Happstack.Server.Types (Conf, port)


data NetworkConfig = NetworkConfig
    { netHost :: String
    , netPort :: Int
    , hostUri :: String
    } deriving (Eq, Show)

networkConfigParser :: IniParser NetworkConfig
networkConfigParser =
    section "NETWORK" $ do
        host <- fieldOf "host" string
        port <- fieldOf "port" number
        return NetworkConfig {netHost = host, netPort = port, hostUri = "http://" ++ host ++ ":" ++ show port}

customHappstackServerConf :: NetworkConfig -> Conf
customHappstackServerConf netConf =
    nullConf { port = netPort netConf
             }
