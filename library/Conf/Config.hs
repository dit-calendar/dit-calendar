{-# LANGUAGE OverloadedStrings #-}

module Conf.Config
    ( readConfig
    , Config(..)
    , NetworkConfig(..)
    , LocalConfig(..)
    ) where

import           Data.Ini.Config
import           Data.Text

data Config = Config
    { cfNetwork :: NetworkConfig
    , cfLocal   :: Maybe LocalConfig
    } deriving (Eq, Show)

data NetworkConfig = NetworkConfig
    { netHost :: String
    , netPort :: Int
    } deriving (Eq, Show)

data LocalConfig = LocalConfig
    { localUser :: Text
    } deriving (Eq, Show)

configParser :: IniParser Config
configParser = do
    netCf <-
        section "NETWORK" $ do
            host <- fieldOf "host" string
            port <- fieldOf "port" number
            return NetworkConfig {netHost = host, netPort = port}
    locCf <- sectionMb "LOCAL" $ LocalConfig <$> field "user"
    return Config {cfNetwork = netCf, cfLocal = locCf}

readConfig :: Text -> Either String Config
readConfig textConfig = parseIniFile textConfig configParser
