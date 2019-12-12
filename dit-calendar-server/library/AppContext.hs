{-# LANGUAGE FlexibleInstances #-}
module AppContext (AppContext(..), CtrlV, App, AppReader(..))where

import           Control.Monad.Reader           (asks, local)

import           Happstack.Server               (Response)
import           Web.Routes.RouteT              (RouteT)

import           Conf.Config                    (Config)
import           Data.Repository.Acid.DBState (Acid)
import           Presentation.Route.PageEnum    (Sitemap)
import           Server.HappstackHelper         (FoundationT)

import qualified Data.Domain.User               as DomainUser


type App  = FoundationT AppReader
type CtrlV'   = RouteT Sitemap App
type CtrlV    = CtrlV' Response

data AppReader = AppReader
    {
    acidState     :: Acid
    , config      :: Config
    , currentUser :: Maybe DomainUser.User
    }

updateUserInAppContext :: DomainUser.User -> AppReader -> AppReader
updateUserInAppContext mUser reader = reader { currentUser = Just mUser}

class Monad m => AppContext m  where
    setCurrentUser :: DomainUser.User -> m a -> m a
    getCurrentUser :: m (Maybe DomainUser.User)

instance AppContext App where
    setCurrentUser user = local (updateUserInAppContext user)
    getCurrentUser = asks currentUser
