{-# LANGUAGE FlexibleContexts #-}
module Auth.Authorization ( callIfAuthorized ) where

import           Control.Monad.IO.Class             (liftIO)
import           Data.Text                          (unpack)
import           Data.Time                          (getCurrentTime)

import           Happstack.Authenticate.Core        (Token (_tokenUser),
                                                     decodeAndVerifyToken)
import           Happstack.Foundation               (HasAcidState (getAcidState))
import           Happstack.Server                   (getHeaderM,
                                                     internalServerError,
                                                     toResponse, unauthorized)

import           Data.Repository.Acid.CalendarEntry (MonadDBCalendar)
import           Data.Repository.Acid.Task          (MonadDBTask)
import           Presentation.AcidHelper            (CtrlV, CtrlV')
import           Presentation.Route.PageEnum        (Sitemap (..))

import qualified Data.ByteString.Char8              as B
import qualified Data.Domain.User                   as DomainUser
import qualified Data.Repository.UserRepo           as UserRepo
import qualified Data.Text.Encoding                 as T
import qualified Happstack.Authenticate.Core        as AuthUser


callIfAuthorized ::(MonadDBCalendar CtrlV', MonadDBTask CtrlV') => (DomainUser.User -> CtrlV) -> CtrlV
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
                            Just u -> route u

verifyToken auth' = do
    let auth = B.drop 7 auth'
    now <- liftIO getCurrentTime
    authenticateState <- getAcidState
    decodeAndVerifyToken authenticateState now (T.decodeUtf8 auth)

getDomainUser :: (MonadDBCalendar CtrlV', MonadDBTask CtrlV') => AuthUser.User -> CtrlV' (Maybe DomainUser.User)
getDomainUser (AuthUser.User _ name _) = UserRepo.findUserByName $ unpack (AuthUser._unUsername name)

responseWithError :: AuthUser.User -> CtrlV
responseWithError (AuthUser.User _ name _) = internalServerError $ toResponse ("something went wrong. Domainuser: "
                                                                   ++ unpack (AuthUser._unUsername name) ++ " not found")
