module Controller.HomeController where

import Route.PageEnum as Page
import Domain.User as User
import Controller.UserController as UserController

import Prelude                 hiding ( head )

import Happstack.Server  ( Response, ServerPartT
                          , ok, toResponse, lookRead )
import Web.Routes        ( RouteT, runRouteT, Site(..)
                          , setDefault, mkSitePI )
import Web.Routes.Happstack    ( implSite )
import Data.Acid            ( AcidState )
import Data.Acid.Advanced   ( query', update' )

--handler for homePage
getHomePage :: AcidState User.UserList -> RouteT Page.SiteMap (ServerPartT IO) Response
getHomePage = UserController.getUsersPage