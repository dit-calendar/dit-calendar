module Auth ( api, authenticateConfig, passwordConfig ) where

import Control.Monad.IO.Class   ( liftIO )
import Data.Time                ( getCurrentTime )
import Data.Acid                ( AcidState )
import Data.Text                ( pack )
import Data.Maybe

import Happstack.Server                     ( unauthorized, getHeaderM, ok, toResponse, ServerPartT, Response, ServerMonad )
import Happstack.Authenticate.Core          ( AuthenticateState, decodeAndVerifyToken, AuthenticateConfig(..)
                                            , usernamePolicy )
import Happstack.Authenticate.Password.Core ( PasswordConfig(..) )

import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding    as T
import qualified Data.Text             as T


authenticateConfig :: AuthenticateConfig
authenticateConfig = AuthenticateConfig
             { _isAuthAdmin        = const $ return True
             , _usernameAcceptable = usernamePolicy
             , _requireEmail       = True
             }

passwordConfig :: PasswordConfig
passwordConfig = PasswordConfig
             { _resetLink = pack "http://localhost:8000/#resetPassword"
             , _domain    = pack "example.org"
             , _passwordAcceptable = \t ->
                 if T.length t >= 5
                 then Nothing
                 else Just $ pack "Must be at least 5 characters."
             }

api :: AcidState AuthenticateState -> ServerPartT IO Response
api authenticateState =
  do mAuth <- getHeaderM "Authorization"
     case mAuth of
       Nothing -> unauthorized $ toResponse "You are not authorized."
       (Just auth') ->
         do let auth = B.drop 7 auth'
            now <- liftIO getCurrentTime
            mToken <- decodeAndVerifyToken authenticateState now (T.decodeUtf8 auth)
            case mToken of
              Nothing    -> unauthorized $ toResponse "You are not authorized."
              (Just _)   -> ok $ toResponse "You are now authorized"
