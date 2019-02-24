module Conf.AuthConf ( authenticateConfig, passwordConfig, tlsConf ) where

import           Happstack.Authenticate.Core          (AuthenticateConfig (..),
                                                       usernamePolicy)
import           Happstack.Authenticate.Password.Core (PasswordConfig (..))
import           Happstack.Server.SimpleHTTPS         (TLSConf (..),
                                                       nullTLSConf)

import qualified Data.Text                            as T

authenticateConfig :: AuthenticateConfig
authenticateConfig = AuthenticateConfig
    { _isAuthAdmin        = const $ return True
    , _usernameAcceptable = usernamePolicy
    , _requireEmail       = True
    }

passwordConfig :: PasswordConfig
passwordConfig = PasswordConfig
    { _resetLink = T.pack "https://localhost:8443/#resetPassword"
    , _domain    = T.pack "example.org"
    , _passwordAcceptable = \t ->
     if T.length t >= 5
     then Nothing
     else Just $ T.pack "Must be at least 5 characters."
    }

tlsConf :: TLSConf
tlsConf =
    nullTLSConf { tlsPort = 8443
                , tlsCert = "library/Auth/ssl/localhost.crt"
                , tlsKey  = "library/Auth/ssl/localhost.key"
                }
