module Conf.AuthConf ( authenticateConfig, passwordConfig, tlsConf ) where

import           Happstack.Authenticate.Core          (AuthenticateConfig (..),
                                                       usernamePolicy)
import           Happstack.Authenticate.Password.Core (PasswordConfig (..))
import           Happstack.Server.SimpleHTTPS         (TLSConf (..),
                                                       nullTLSConf)
import Conf.Config

import qualified Data.Text                            as T

authenticateConfig :: AuthenticateConfig
authenticateConfig = AuthenticateConfig
    { _isAuthAdmin        = const $ return True
    , _usernameAcceptable = usernamePolicy
    , _requireEmail       = True
    }

passwordConfig :: Config -> PasswordConfig
passwordConfig conf =
    let url = T.pack ("https://" ++ netHost (cfNetwork conf) ++ ":" ++ show (netPort (cfNetwork conf)) ++ "/#resetPassword") in
    PasswordConfig
        { _resetLink = url
        , _domain    = T.pack "example.org"
        , _passwordAcceptable = \t ->
         if T.length t >= 5
         then Nothing
         else Just $ T.pack "Must be at least 5 characters."
        }

tlsConf :: Config -> TLSConf
tlsConf conf =
    nullTLSConf { tlsPort = netPort $ cfNetwork conf
                , tlsCert = "library/Auth/ssl/localhost.crt"
                , tlsKey  = "library/Auth/ssl/localhost.key"
                }
