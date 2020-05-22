{-# LANGUAGE FlexibleContexts #-}
module Auth.Authorization ( callIfAuthorized, createToken ) where

import           Data.Text                   (unpack)

import           Conf.AuthConf               (authenticateConfig)
import           Data.Acid                   (AcidState)
import           Data.Acid.Advanced          (query')
import           Data.Aeson                  (ToJSON)
import           Happstack.Authenticate.Core (AuthenticateState,
                                              GetUserByUsername (..),
                                              Token (_tokenUser), Username (..),
                                              getToken, issueToken,
                                              toJSONSuccess)
import           Happstack.Server            (Response, internalServerError,
                                              toResponse, unauthorized)

import           AppContext                  (App, setCurrentUser)
import           Data.Domain.Types           (EitherResult)
import           Server.HappstackHelper      (HasAcidState (getAcidState))
import           Server.ResponseBuilder      (handleResponse)

import qualified Data.Domain.User            as DomainUser
import qualified Data.Repository.UserRepo    as UserRepo
import qualified Happstack.Authenticate.Core as AuthUser



callIfAuthorized :: ToJSON dto => (DomainUser.User -> App (EitherResult dto)) -> App Response
callIfAuthorized route = do
    mAuth <- getUserToken
    case mAuth of
        Nothing -> unauthorized $ toResponse "You are not authorized."
        Just (token, _) -> do
            let authUser = _tokenUser token
            loggedUser <- getDomainUser authUser
            case loggedUser of
                Nothing -> responseWithError authUser
                Just u  -> setCurrentUser u $ route u >>= handleResponse

getUserToken = do
    authenticateState <- getAcidState
    getToken authenticateState

getDomainUser :: AuthUser.User -> App (Maybe DomainUser.User)
getDomainUser (AuthUser.User _ name _) = UserRepo.findUserByLoginName $ AuthUser._unUsername name

responseWithError :: AuthUser.User -> App Response
responseWithError (AuthUser.User _ name _) = internalServerError $ toResponse ("something went wrong. Domainuser: "
                                                                   ++ unpack (AuthUser._unUsername name) ++ " not found")
createToken :: DomainUser.User -> App Response
createToken domainUser = do
    authState <- getAcidState
    mAuthUser <- query' authState (GetUserByUsername $ Username domainUserName)
    case mAuthUser of
        Just authUser -> toJSONSuccess <$> issueToken authState authenticateConfig authUser
        Nothing -> internalServerError $ toResponse ("internal server error. AuthUser for Domainuser " ++ unpack domainUserName ++ " not found")
    where domainUserName = DomainUser.loginName domainUser
