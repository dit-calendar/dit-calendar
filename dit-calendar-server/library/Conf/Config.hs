{-# LANGUAGE OverloadedStrings #-}

module Conf.Config
    ( readConfig
    , Config(..)
    , LocalConfig(..)
    , CorsConfig(..)
    ) where

import           Data.Ini.Config
import           Data.Text

import           Conf.NetworkConfig (NetworkConfig (..), networkConfigParser)

data Config = Config
    { cfNetwork :: NetworkConfig
    , cfLocal   :: Maybe LocalConfig
    , cfCors     :: CorsConfig
    } deriving (Eq, Show)

data CorsConfig = CorsConfig
    { clientUrl :: String
    , allowCookie :: String
    } deriving (Eq, Show)

data LocalConfig = LocalConfig
    { adminUser     :: Text
    , adminPassword :: Text
    } deriving (Eq, Show)

localConfigParser :: IniParser (Maybe LocalConfig)
localConfigParser = 
    sectionMb "LOCAL" $ do
        user <- fieldOf "admin.user" string
        password <- fieldOf "admin.password" string
        return LocalConfig {adminPassword = password, adminUser = user}
        
corsConfigParser :: IniParser CorsConfig
corsConfigParser = 
    section "CORS" $ do
                url <- fieldOf "allow.origin" string
                cookie <- fieldOf "allow.credentials" string
                return CorsConfig {clientUrl = url, allowCookie = cookie}

configParser :: Maybe String -> IniParser Config
configParser defaultPort = do
    netCf <- networkConfigParser defaultPort
    locCf <- localConfigParser
    corsCf <- corsConfigParser
    return Config {cfNetwork = netCf, cfLocal = locCf, cfCors = corsCf}

readConfig :: Maybe String -> Text -> Either String Config
readConfig defaultPort textConfig = parseIniFile textConfig $ configParser defaultPort
