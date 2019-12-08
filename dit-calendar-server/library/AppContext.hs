module AppContext (CtrlV, App, AppContext(..), setCurrentUser, getCurrentUser)where

import           Control.Monad.Reader           (asks, local)

import           Happstack.Server               (Response)
import           Web.Routes.RouteT              (RouteT)

import           Conf.Config                    (Config)
import           Data.Repository.Acid.DBState (Acid)
import           Presentation.Route.PageEnum    (Sitemap)
import           Server.HappstackHelper         (FoundationT)

import qualified Data.Domain.User               as DomainUser


data AppContext = AppContext
    {
    acidState     :: Acid
    , config      :: Config
    , currentUser :: Maybe DomainUser.User
    }

updateUserInAppContext :: DomainUser.User -> AppContext -> AppContext
updateUserInAppContext mUser reader = reader { currentUser = Just mUser}

setCurrentUser :: DomainUser.User -> App m -> App m
setCurrentUser user = local (updateUserInAppContext user)

getCurrentUser :: App (Maybe DomainUser.User)
getCurrentUser = asks currentUser

type App  = FoundationT AppContext ()
type CtrlV'   = RouteT Sitemap App
type CtrlV    = CtrlV' Response
