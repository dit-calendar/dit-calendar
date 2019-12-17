{-# LANGUAGE OverloadedStrings #-}

module Conf.NetworkConfig (NetworkConfig(..), customHappstackServerConf, networkConfigParser) where

import           Data.Ini.Config

import           Data.Maybe             (fromMaybe)
import           Happstack.Server       (nullConf)
import           Happstack.Server.Types (Conf, port)


data NetworkConfig = NetworkConfig
    { netHost :: String
    , netPort :: Int
    , hostUri :: String
    } deriving (Eq, Show)

networkConfigParser :: Maybe String -> IniParser NetworkConfig
networkConfigParser mdefaultPort =
    section "NETWORK" $ do
        host <- fieldOf "host" string
        defaultPort <- fieldOf "port" number
        let port = maybe defaultPort read mdefaultPort
        return NetworkConfig {netHost = host, netPort = port, hostUri = "http://" ++ host ++ ":" ++ show port}

customHappstackServerConf :: NetworkConfig -> Conf
customHappstackServerConf netConf =
    nullConf { port = netPort netConf
             }
