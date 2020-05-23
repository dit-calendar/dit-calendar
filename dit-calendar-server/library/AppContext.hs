{-# LANGUAGE FlexibleInstances #-}
module AppContext (AppContext(..), CtrlV, App, AppReader(..))where

import           Control.Monad.Reader        (asks, local)

import           Happstack.Server            (Response)
import           Web.Routes.RouteT           (RouteT)

import           Conf.Config                 (AppConfig)
import           Presentation.Route.PageEnum (Sitemap)
import           Server.DBState              (Acid)
import           Server.HappstackHelper      (FoundationT)

import qualified Data.Domain.User            as DomainUser


type App  = FoundationT AppReader
type CtrlV'   = RouteT Sitemap App
type CtrlV    = CtrlV' Response

data AppReader = AppReader
    {
    acidState     :: Acid
    , config      :: AppConfig
    , currentUser :: Maybe DomainUser.User
    }

updateUserInAppContext :: DomainUser.User -> AppReader -> AppReader
updateUserInAppContext mUser reader = reader { currentUser = Just mUser}

class Monad m => AppContext m  where
    setCurrentUser :: DomainUser.User -> m a -> m a
    getCurrentUser :: m (Maybe DomainUser.User)
    getConfig      :: m AppConfig

instance AppContext App where
    setCurrentUser user = local (updateUserInAppContext user)
    getCurrentUser = asks currentUser
    getConfig = asks config

instance AppContext CtrlV' where
    setCurrentUser user = local (updateUserInAppContext user)
    getCurrentUser = asks currentUser
    getConfig = asks config

