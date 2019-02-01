{-# LANGUAGE FlexibleContexts #-}
module Auth.Authorization ( callIfAuthorized ) where

import           Control.Monad.IO.Class             (liftIO)
import           Data.Text                          (unpack)
import           Data.Time                          (getCurrentTime)

import           Happstack.Authenticate.Core        (Token (_tokenUser),
                                                     decodeAndVerifyToken)
import           Happstack.Foundation               (HasAcidState (getAcidState))
import           Happstack.Server                   (Response, getHeaderM,
                                                     internalServerError,
                                                     toResponse, unauthorized)

import           AcidHelper            (App)
import           Presentation.Route.PageEnum        (Sitemap (..))

import qualified Data.ByteString.Char8              as B
import qualified Data.Domain.User                   as DomainUser
import qualified Data.Repository.UserRepo           as UserRepo
import qualified Data.Text.Encoding                 as T
import qualified Happstack.Authenticate.Core        as AuthUser


callIfAuthorized :: (DomainUser.User -> App Response) -> App Response
callIfAuthorized route = do
    mAuth <- getHeaderM "Authorization"
    case mAuth of
        Nothing -> unauthorized $ toResponse "You are not authorized."
        (Just auth') ->
            do  mToken <- verifyToken auth'
                case mToken of
                    Nothing -> unauthorized $ toResponse "You are not authorized!"
                    Just (token,_) -> do
                        let authUser = _tokenUser token
                        loggedUser <- getDomainUser authUser
                        case loggedUser of
                            Nothing -> responseWithError authUser
                            Just u  -> route u

verifyToken auth' = do
    let auth = B.drop 7 auth'
    now <- liftIO getCurrentTime
    authenticateState <- getAcidState
    decodeAndVerifyToken authenticateState now (T.decodeUtf8 auth)

getDomainUser :: AuthUser.User -> App (Maybe DomainUser.User)
getDomainUser (AuthUser.User _ name _) = UserRepo.findUserByLoginName $ AuthUser._unUsername name

responseWithError :: AuthUser.User -> App Response
responseWithError (AuthUser.User _ name _) = internalServerError $ toResponse ("something went wrong. Domainuser: "
                                                                   ++ unpack (AuthUser._unUsername name) ++ " not found")
