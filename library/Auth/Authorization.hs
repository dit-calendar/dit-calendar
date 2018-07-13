{-# LANGUAGE FlexibleContexts     #-}
module Auth.Authorization ( getLoggedUser, callIfAuthorized ) where

import           Control.Monad.IO.Class      (liftIO)
import           Data.Acid                   (AcidState)
import           Data.Maybe                  (fromJust)
import           Data.Time                   (getCurrentTime)
import           Data.Text                   (unpack)

import           Happstack.Authenticate.Core (AuthenticateState,
                                              Token (_tokenUser),
                                              decodeAndVerifyToken)
import           Happstack.Foundation        (query)
import           Happstack.Server            (getHeaderM, toResponse,
                                              unauthorized)

import           Presentation.AcidHelper     (CtrlV, CtrlV')
import           Presentation.Route.PageEnum (Sitemap (..))
import           Data.Repository.Acid.CalendarEntry (MonadDBCalendar)
import           Data.Repository.Acid.Task          (MonadDBTask)

import qualified Data.ByteString.Char8       as B
import qualified Data.Domain.User            as DomainUser
import qualified Data.Repository.UserRepo    as UserRepo
import qualified Data.Text.Encoding          as T
import qualified Happstack.Authenticate.Core as AuthUser


callIfAuthorized ::(MonadDBCalendar CtrlV', MonadDBTask CtrlV') => AcidState AuthenticateState -> (DomainUser.User -> CtrlV) -> CtrlV
callIfAuthorized authenticateState route =
    do  mAuth <- getHeaderM "Authorization"
        case mAuth of
            Nothing -> unauthorized $ toResponse "You are not authorized."
            (Just auth') ->
                do  let auth = B.drop 7 auth'
                    now <- liftIO getCurrentTime
                    mToken <- decodeAndVerifyToken authenticateState now (T.decodeUtf8 auth)
                    case mToken of
                        Nothing -> unauthorized $ toResponse "You are not authorized!"
                        (Just (token,_)) -> getLoggedUser (_tokenUser token) route
                        --(Just (token,_)) -> route url (getLoggedUser (_tokenUser token))

getLoggedUser :: (MonadDBCalendar CtrlV', MonadDBTask CtrlV') => AuthUser.User -> (DomainUser.User -> CtrlV) -> CtrlV
getLoggedUser (AuthUser.User _ name _) route = do
                            mUser <- UserRepo.findUserByName $ unpack (AuthUser._unUsername name)
                            route $ fromJust mUser
