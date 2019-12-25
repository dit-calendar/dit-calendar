{-# LANGUAGE FlexibleContexts #-}
module Auth.Authorization ( callIfAuthorized ) where

import           Data.Text                   (unpack)

import           Data.Aeson                  (ToJSON)
import           Happstack.Authenticate.Core (Token (_tokenUser), getToken)
import           Happstack.Server            (Response, getHeaderM,
                                              internalServerError, toResponse,
                                              unauthorized)

import           AppContext                  (App, setCurrentUser)
import           Data.Domain.Types           (EitherResult)
import           Presentation.Route.PageEnum (Sitemap (..))
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
